/* sqlite VFS on a raw block device, see blockdev.h
   - block 0 is a superblock with magic and layout, see sqv_format
   - two regions of 4K blocks: journal, then db
   - first block of a region holds magic + file size, written on sync
   - delete = size 0, so DELETE journal mode works as is
   - one connection, no locking, temp files in memory (SQLITE_TEMP_STORE=3)
   - https://sqlite.org/vfs.html, https://sqlite.org/c3ref/io_methods.html */

#include "sqlite3.h"

#define SQV_BLK        4096
#define SQV_SECT       (SQV_BLK / 512)
#define SQV_MAGIC      0x72716c73  // "sqlr"
#define SQV_JOURNAL_NB 16384       // 64MB
#define SQV_SB_MAGIC   "uc_tools sqlv1"

struct sqv_superblock {
    char magic[16];
    uint32_t block_size;
    uint32_t journal_blk0, journal_nb_blk;
    uint32_t db_blk0, db_nb_blk;
};

struct sqv_region {
    uint32_t blk0;    // header block, data follows
    uint32_t nb_blk;  // including header
    uint64_t size;
    int dirty;
};
struct sqv_file {
    sqlite3_file base;
    struct sqv_region *r;
};
struct sqv {
    struct blockdev *bd;
    uint8_t buf[SQV_BLK];
    uint32_t cached;  // block in buf, or -1
    struct sqv_region journal, db;
    uint32_t crash_after;  // db writes, test only: crash on the last one
    void (*crash)(void);   // or hang if 0
};
struct sqv sqv;

static int sqv_read_blk(uint32_t b) {
    if (sqv.cached == b) return 0;
    sqv.cached = -1;
    if (sqv.bd->read(sqv.bd, b * SQV_SECT, sqv.buf, SQV_SECT)) return -1;
    sqv.cached = b;
    return 0;
}
static int sqv_write_blk(uint32_t b) {
    sqv.cached = -1;
    if (sqv.bd->write(sqv.bd, b * SQV_SECT, sqv.buf, SQV_SECT)) return -1;
    sqv.cached = b;
    return 0;
}
static int sqv_write_header(struct sqv_region *r) {
    uint32_t *w = (uint32_t *)sqv.buf;
    mini_memset(w, 0, SQV_BLK);
    w[0] = SQV_MAGIC;
    w[1] = r->size;
    w[2] = r->size >> 32;
    if (sqv_write_blk(r->blk0)) return SQLITE_IOERR_WRITE;
    r->dirty = 0;
    return SQLITE_OK;
}
static void sqv_read_header(struct sqv_region *r) {
    uint32_t *w = (uint32_t *)sqv.buf;
    r->size = 0;
    if (sqv_read_blk(r->blk0) || w[0] != SQV_MAGIC) return;
    r->size = w[1] | ((uint64_t)w[2] << 32);
}
static uint64_t sqv_max(struct sqv_region *r) {
    return (uint64_t)(r->nb_blk - 1) * SQV_BLK;
}

static int sqv_close(sqlite3_file *f) {
    return SQLITE_OK;
}
static int sqv_read(sqlite3_file *f, void *buf, int amt, sqlite3_int64 off) {
    struct sqv_region *r = ((struct sqv_file *)f)->r;
    uint8_t *dst = buf;
    int rv = SQLITE_OK;
    if (off + amt > r->size) {
        mini_memset(buf, 0, amt);
        amt = off < r->size ? r->size - off : 0;
        rv = SQLITE_IOERR_SHORT_READ;
    }
    while (amt > 0) {
        uint32_t o = off % SQV_BLK;
        uint32_t n = SQV_BLK - o < amt ? SQV_BLK - o : amt;
        if (sqv_read_blk(r->blk0 + 1 + off / SQV_BLK)) return SQLITE_IOERR_READ;
        mini_memcpy(dst, sqv.buf + o, n);
        dst += n; off += n; amt -= n;
    }
    return rv;
}
static int sqv_write(sqlite3_file *f, const void *buf, int amt, sqlite3_int64 off) {
    struct sqv_region *r = ((struct sqv_file *)f)->r;
    const uint8_t *src = buf;
    if (off + amt > sqv_max(r)) return SQLITE_FULL;
    if (r == &sqv.db && sqv.crash_after && !--sqv.crash_after) {
        LOG("sqv crash\n");
        if (sqv.crash) sqv.crash();
        for (;;) __asm__ volatile ("cli; hlt");
    }
    if (off + amt > r->size) { r->size = off + amt; r->dirty = 1; }
    while (amt > 0) {
        uint32_t b = r->blk0 + 1 + off / SQV_BLK;
        uint32_t o = off % SQV_BLK;
        uint32_t n = SQV_BLK - o < amt ? SQV_BLK - o : amt;
        if (n < SQV_BLK && sqv_read_blk(b)) return SQLITE_IOERR_READ;
        mini_memcpy(sqv.buf + o, src, n);
        if (sqv_write_blk(b)) return SQLITE_IOERR_WRITE;
        src += n; off += n; amt -= n;
    }
    return SQLITE_OK;
}
static int sqv_truncate(sqlite3_file *f, sqlite3_int64 size) {
    struct sqv_region *r = ((struct sqv_file *)f)->r;
    if (size < r->size) { r->size = size; r->dirty = 1; }
    return SQLITE_OK;
}
static int sqv_sync(sqlite3_file *f, int flags) {
    struct sqv_region *r = ((struct sqv_file *)f)->r;
    if (r->dirty && sqv_write_header(r)) return SQLITE_IOERR_FSYNC;
    return sqv.bd->flush(sqv.bd) ? SQLITE_IOERR_FSYNC : SQLITE_OK;
}
static int sqv_file_size(sqlite3_file *f, sqlite3_int64 *size) {
    *size = ((struct sqv_file *)f)->r->size;
    return SQLITE_OK;
}
static int sqv_lock(sqlite3_file *f, int lock) {
    return SQLITE_OK;
}
static int sqv_check_reserved_lock(sqlite3_file *f, int *out) {
    *out = 0;
    return SQLITE_OK;
}
static int sqv_file_control(sqlite3_file *f, int op, void *arg) {
    return SQLITE_NOTFOUND;
}
static int sqv_sector_size(sqlite3_file *f) {
    return SQV_BLK;
}
static int sqv_device_characteristics(sqlite3_file *f) {
    return 0;
}
static const sqlite3_io_methods sqv_io = {
    .iVersion               = 1,
    .xClose                 = sqv_close,
    .xRead                  = sqv_read,
    .xWrite                 = sqv_write,
    .xTruncate              = sqv_truncate,
    .xSync                  = sqv_sync,
    .xFileSize              = sqv_file_size,
    .xLock                  = sqv_lock,
    .xUnlock                = sqv_lock,
    .xCheckReservedLock     = sqv_check_reserved_lock,
    .xFileControl           = sqv_file_control,
    .xSectorSize            = sqv_sector_size,
    .xDeviceCharacteristics = sqv_device_characteristics,
};

/* Name is only used to tell the journal from the db. */
static struct sqv_region *sqv_region(const char *name) {
    uint32_t n = mini_strlen(name);
    if (n >= 8 && !mini_strcmp(name + n - 8, "-journal")) return &sqv.journal;
    return &sqv.db;
}
static int sqv_open(sqlite3_vfs *vfs, const char *name, sqlite3_file *f,
                    int flags, int *out_flags) {
    struct sqv_file *sf = (struct sqv_file *)f;
    if (!sqv.bd || !name ||
        !(flags & (SQLITE_OPEN_MAIN_DB | SQLITE_OPEN_MAIN_JOURNAL)))
        return SQLITE_CANTOPEN;
    sf->r = sqv_region(name);
    f->pMethods = &sqv_io;
    if (out_flags) *out_flags = flags;
    return SQLITE_OK;
}
static int sqv_delete(sqlite3_vfs *vfs, const char *name, int sync_dir) {
    struct sqv_region *r = sqv_region(name);
    r->size = 0;
    if (sqv_write_header(r) || sqv.bd->flush(sqv.bd)) return SQLITE_IOERR_DELETE;
    return SQLITE_OK;
}
static int sqv_access(sqlite3_vfs *vfs, const char *name, int flags, int *out) {
    *out = sqv_region(name)->size > 0;
    return SQLITE_OK;
}
static int sqv_full_pathname(sqlite3_vfs *vfs, const char *name, int n, char *out) {
    sqlite3_snprintf(n, out, "%s", name);
    return SQLITE_OK;
}
static int sqv_randomness(sqlite3_vfs *vfs, int n, char *out) {
    for (int i = 0; i < n; i++) {
        uint32_t lo, hi;
        __asm__ volatile ("rdtsc" : "=a"(lo), "=d"(hi));
        out[i] = lo ^ (lo >> 8);
    }
    return n;
}
static int sqv_sleep(sqlite3_vfs *vfs, int us) {
    return us;
}
static int sqv_current_time_int64(sqlite3_vfs *vfs, sqlite3_int64 *t) {
    *t = 210866760000000LL;  // 1970-01-01, no rtc yet
    return SQLITE_OK;
}
static int sqv_current_time(sqlite3_vfs *vfs, double *t) {
    *t = 2440587.5;
    return SQLITE_OK;
}
static int sqv_get_last_error(sqlite3_vfs *vfs, int n, char *buf) {
    return 0;
}
static sqlite3_vfs sqv_vfs = {
    .iVersion            = 2,
    .szOsFile            = sizeof(struct sqv_file),
    .mxPathname          = 64,
    .zName               = "blockdev",
    .xOpen               = sqv_open,
    .xDelete             = sqv_delete,
    .xAccess             = sqv_access,
    .xFullPathname       = sqv_full_pathname,
    .xRandomness         = sqv_randomness,
    .xSleep              = sqv_sleep,
    .xCurrentTime        = sqv_current_time,
    .xGetLastError       = sqv_get_last_error,
    .xCurrentTimeInt64   = sqv_current_time_int64,
};
int sqlite3_os_init(void) {
    return sqlite3_vfs_register(&sqv_vfs, 1);
}
int sqlite3_os_end(void) {
    return SQLITE_OK;
}

/* Reads the layout from the superblock and the region headers. */
static int sqv_init(struct blockdev *bd) {
    struct sqv_superblock *sb = (struct sqv_superblock *)sqv.buf;
    sqv.bd = 0;
    sqv.cached = -1;
    if (bd->read(bd, 0, sqv.buf, SQV_SECT) ||
        mini_memcmp(sb->magic, SQV_SB_MAGIC, sizeof(SQV_SB_MAGIC)) ||
        sb->block_size != SQV_BLK) {
        LOG("sqv no superblock, see sqformat\n");
        return -1;
    }
    sqv.journal = (struct sqv_region){ .blk0 = sb->journal_blk0,
                                       .nb_blk = sb->journal_nb_blk };
    sqv.db = (struct sqv_region){ .blk0 = sb->db_blk0,
                                  .nb_blk = sb->db_nb_blk };
    sqv.bd = bd;
    sqv_read_header(&sqv.journal);
    sqv_read_header(&sqv.db);
    LOG("sqv journal=%d db=%d\n",
        (uint32_t)sqv.journal.size, (uint32_t)sqv.db.size);
    return 0;
}
/* Writes the superblock and empty region headers.  Destroys any
   database on bd. */
static int sqv_format(struct blockdev *bd) {
    uint32_t nb_blk = bd->nb_sectors / SQV_SECT;
    if (nb_blk <= 2 * SQV_JOURNAL_NB) {
        LOG("sqv device too small\n");
        return -1;
    }
    sqv.bd = bd;
    sqv.journal = (struct sqv_region){ .blk0 = 1, .nb_blk = SQV_JOURNAL_NB };
    sqv.db = (struct sqv_region){ .blk0 = 1 + SQV_JOURNAL_NB,
                                  .nb_blk = nb_blk - 1 - SQV_JOURNAL_NB };
    if (sqv_write_header(&sqv.journal) || sqv_write_header(&sqv.db)) goto fail;
    struct sqv_superblock *sb = (struct sqv_superblock *)sqv.buf;
    mini_memset(sqv.buf, 0, SQV_BLK);
    mini_memcpy(sb->magic, SQV_SB_MAGIC, sizeof(SQV_SB_MAGIC));
    sb->block_size     = SQV_BLK;
    sb->journal_blk0   = sqv.journal.blk0;
    sb->journal_nb_blk = sqv.journal.nb_blk;
    sb->db_blk0        = sqv.db.blk0;
    sb->db_nb_blk      = sqv.db.nb_blk;
    if (sqv_write_blk(0) || bd->flush(bd)) goto fail;
    LOG("sqv formatted blocks=%d\n", nb_blk);
    sqv.bd = 0;
    return 0;
  fail:
    sqv.bd = 0;
    return -1;
}
