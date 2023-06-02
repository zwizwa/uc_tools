// Wrap log_parse log iterator as an sqlite3 table.

// See examples:
// https://github.com/ralight/sqlite3-pcre/blob/master/pcre.c
// https://www.sqlite.org/loadext.html -> 4.1
// https://www.sqlite.org/carray.html
// https://www.sqlite.org/src/file/ext/misc
// https://www.sqlite.org/vtab.html#tabfunc2
// https://www.sqlite.org/vtablist.html

// SQL side looks like:

// The extension module file is logparse.so
// sqlite> .load ./logparse

// CREATE VIRTUAL TABLE lp USING logparse("dev1.bin");

#define _GNU_SOURCE         /* See feature_test_macros(7) */
#include <sys/mman.h>



#include "log_parse.h"
#include <sqlite3ext.h>

#include "mmap_file.h"

SQLITE_EXTENSION_INIT1

/* The 'base' member contains the base class.  Must be first */
struct logparse_table {
    sqlite3_vtab base;
    struct mmap_file file;
    int64_t ts_sync;
    uint8_t synced:1;
};

struct logparse_cursor {
    sqlite3_vtab_cursor base;
    struct log_parse lp;
    struct log_parse_cbs lp_cbs;

    uint32_t rowid;

    /* Callbacks are invoked in the extent as part of xNext calling
       log_parse_continue().  Those callbacks will save the current
       framing here, later picked up by xColumn */
    const uint8_t *line;
    uintptr_t len;
    uint32_t ts;
    uint32_t ts_prev;
    uint32_t ts_wraps;
    uint8_t bin:1;

    /* xNext calling log_parse_continue() can determine end-of-file
       condition, which is saved ere and picked up by xEof */
    uint8_t eof:1;

};

static struct logparse_cursor *logparse_cursor(sqlite3_vtab_cursor *p) {
    return (void*)p;
}
static struct logparse_table *logparse_table(sqlite3_vtab *p) {
    return (void*)p;
}

static void logparse_cursor_update_ts(struct logparse_cursor *cur, uint32_t ts) {
    cur->ts_prev = cur->ts;
    cur->ts = ts;
    if (cur->ts_prev > cur->ts) {
        /* Counter wrapped.  This function is called exactly once per
           line so we can keep track of the number of wraps by
           incrementing here.  Note that microcontroller needs to
           ensure there are enough log messages such that there is not
           more than one counter wrap per wrap we can detect this
           way. */
        //LOG("ts_wrap 0x%x 0x%x\n", cur->ts_prev, cur->ts);
        cur->ts_wraps++;
    }
}
int64_t logparse_cursor_ts(struct logparse_cursor *cur) {
    int64_t ts = cur->ts_wraps;
    ts = (ts << 32) + cur->ts;
    return ts;
}
static log_parse_status_t ts_cb(struct log_parse *s, uint32_t ts,
                                const uint8_t *line, uintptr_t len,
                                int bin) {
    //LOG("ts_line_cb 0x%08x %p %d\n", ts, line, len);
    struct logparse_cursor *cur = STRUCT_FROM_FIELD(struct logparse_cursor, lp, s);
    cur->len = len;
    cur->line = line;
    cur->bin = bin;
    logparse_cursor_update_ts(cur, ts);
    return LOG_PARSE_STATUS_YIELD;
}

/* Callbacks for normal iteration */
static log_parse_status_t ts_line_cb(
    struct log_parse *s, uint32_t ts,
    const uint8_t *line, uintptr_t len) {
    return ts_cb(s,ts,line,len,0);
}
static log_parse_status_t ts_bin_cb(
    struct log_parse *s, uint32_t ts,
    const uint8_t *line, uintptr_t len) {
    return ts_cb(s,ts,line,len,1);
}

/* Callback for sync scan */

static log_parse_status_t ts_bin_sync_cb(
    struct log_parse *s, uint32_t ts,
    const uint8_t *line, uintptr_t len) {
    LOG("sync_cb 0x%08x %d\n", ts, len);


    log_parse_status_t rv = ts_cb(s,ts,line,len,1);
    (void)rv; // we return something else
    struct logparse_cursor *cur = STRUCT_FROM_FIELD(struct logparse_cursor, lp, s);
    struct logparse_table *tab = logparse_table(cur->base.pVtab);
    if ((len == 2) &&
        (line[0] == 0) &&
        (line[1] == 0)) {
        goto got_sync;
    }
    else if ((len == 7) &&
             (!strncmp("ping 0\n", (char*)line, 7))) {
        /* This one is easier to test with a text file. */
        goto got_sync;
    }
    else {
        return LOG_PARSE_STATUS_CONTINUE;
    }
  got_sync:
    tab->ts_sync = logparse_cursor_ts(cur);
    LOG("got sync 0x%x\n", tab->ts_sync);
    return LOG_PARSE_STATUS_YIELD;
}

// The xConnect method is very similar to xCreate. It has the same
// parameters and constructs a new sqlite3_vtab structure just like
// xCreate. And it must also call sqlite3_declare_vtab() like
// xCreate. It should also make all of the same sqlite3_vtab_config()
// calls as xCreate.
//
// The difference is that xConnect is called to establish a new
// connection to an existing virtual table whereas xCreate is called
// to create a new virtual table from scratch.
static int xConnect(
    sqlite3 *db,
    void *pAux,
    int argc, const char *const*argv,
    sqlite3_vtab **ppVtab,
    char **pzErr){
    // LOG("xConnect\n");
    for(int i=0; i<argc; i++) {
        LOG("arg%d %s\n", i, argv[i]);
    }
    // "logparse" "temp" "lp" "dev1.bin"

    struct logparse_table *pNew = sqlite3_malloc(sizeof(*pNew));
    memset(pNew,0,sizeof(*pNew));

    // This contains syntax, e.g. for strings the quotes are included.
    // We just assume quotes are there, and that filenames do not
    // contain quotes. FIXME: Is there a reusable parser for this?
    ASSERT(argc >= 4);
    int n = strlen(argv[3])-2+1;
    char filename[n];
    memcpy(filename, argv[3]+1, n-1);
    filename[n-1] = 0;
    LOG("logparse %s\n", filename);

    mmap_file_open_ro(&pNew->file, filename);
    ASSERT(pNew->file.buf);

    *ppVtab = &pNew->base;
    int rv = sqlite3_declare_vtab(
        db,
        "CREATE TABLE x("
        "ts   INTEGER,"
        "bin  INTEGER,"
        "line TEXT,"
        "schema HIDDEN"
        ")");
    ASSERT(rv == SQLITE_OK);
    return SQLITE_OK;
}
// create a new instance of a virtual table
static int xCreate(
    sqlite3 *db,
    void *pAux,
    int argc, const char *const*argv,
    sqlite3_vtab **ppVtab,
    char **pzErr){
    //LOG("xCreate\n");
    return xConnect(db, pAux, argc, argv, ppVtab, pzErr);
}
static int xDisconnect(sqlite3_vtab *pVtab) {
    return SQLITE_OK;
}

static int xBestIndex(sqlite3_vtab *pVTab, sqlite3_index_info *pII) {
    //LOG("xBestIndex\n");
    return SQLITE_OK;
}

static int xClose(sqlite3_vtab_cursor *pCur) {
    //LOG("xClose\n");
    sqlite3_free(pCur);
    return SQLITE_OK;
}
static int xEof(sqlite3_vtab_cursor *pCur) {
    int eof = logparse_cursor(pCur)->eof;
    //LOG("xEof %d\n", eof);
    return eof;
}
static int xFilter(sqlite3_vtab_cursor *pCur, int idxNum, const char *idxStr,
                   int argc, sqlite3_value **argv) {
    //LOG("xFilter\n");
    return SQLITE_OK;
}
static int xNext(sqlite3_vtab_cursor *pCur) {
    //LOG("xNext\n");
    struct logparse_cursor *cur = logparse_cursor(pCur);
    //struct logparse_table *tab = logparse_table(cur->base.pVtab);
    struct log_parse *lp = &cur->lp;

    log_parse_status_t s = log_parse_continue(lp);
    //LOG("xNext %d\n", s);
    if (s == LOG_PARSE_STATUS_YIELD) {
        /* It found something and passed it to callback. */
        cur->eof = 0;
    }
    else {
        /* It ran off the end, get some more. */
        cur->eof = 1;
        cur->rowid++;
    }
    return SQLITE_OK;
}

static void logparse_cursor_init(struct logparse_cursor *cur,
                                 struct logparse_table *tab) {
    /* All integer values are initialized to 0. */
    memset(cur,0,sizeof(*cur));

    /* SQLite will set this when xOpen finishes, but we rely on it
       during sync scan so initialize it here. */
    cur->base.pVtab = &tab->base;

    struct log_parse *lp = &cur->lp;

    /* Connect log_parse to the memory-mapped file.

       The log_parse_init() takes a non-NULL pointer in case the
       memory layout is stable. In that case, lp->in_mark can be used. */
    log_parse_init(lp, tab->file.buf);

    /* We can leave the input connected permanently.  I.e. we don't
       log_parse_write() */
    lp->in     = tab->file.buf;
    lp->in_len = tab->file.size;

    /* Callbacks invoked on parsed log messages. */
    cur->lp_cbs.line    = ts_line_cb;
    cur->lp_cbs.ts_line = ts_line_cb;
    cur->lp_cbs.ts_bin  = ts_bin_cb;
    lp->cb = &cur->lp_cbs;

    /* SQLITE will call xEof xColumn then xNext xEof xColumn etc...
       So we need to call it once here to get started. */
    xNext(&cur->base);
}

static int xOpen(sqlite3_vtab *pVTab, sqlite3_vtab_cursor **ppCursor) {
    //LOG("xOpen\n");
    struct logparse_table *tab = logparse_table(pVTab);
    struct logparse_cursor *cur = sqlite3_malloc(sizeof(*cur));

    if (!tab->synced) {
        /* If the timebase is not yet synchronized, perform a scan
           here first to find the sync message and record the sync
           offset in the vtab so xColumn can present time-shifted
           timestamps.  This is done here lazily so we can re-use the
           cursor data structure. */
        logparse_cursor_init(cur,tab);
        cur->lp_cbs.line    = ts_bin_sync_cb;
        cur->lp_cbs.ts_line = ts_bin_sync_cb;
        cur->lp_cbs.ts_bin  = ts_bin_sync_cb;
        struct log_parse *lp = &cur->lp;
        log_parse_status_t s = log_parse_continue(lp);
        switch(s) {
        case LOG_PARSE_STATUS_END:
            LOG("no sync\n");
            break;
        case LOG_PARSE_STATUS_YIELD:
            break;
        default:
            ERROR("unexpected status %d\n", s);
        }
        /* This means sync was attempted.  If there was no sync
           message found the sync offset is 0. */
        tab->synced = 1;
    }

    logparse_cursor_init(cur,tab);


    *ppCursor = &cur->base;
    return SQLITE_OK;
}

static int xColumn(sqlite3_vtab_cursor *pCur, sqlite3_context *c, int N) {
    // LOG("xColumn %d\n", N);
    struct logparse_cursor *cur = logparse_cursor(pCur);
    switch(N) {
    case 0: {
        struct logparse_table *tab = logparse_table(cur->base.pVtab);
        uint64_t ts = logparse_cursor_ts(cur) - tab->ts_sync;
        sqlite3_result_int64(c, ts);
        break;
    }
    case 1: {
        sqlite3_result_int(c, cur->bin);
        break;
    }
    case 2: {
        // SQLITE_STATIC means the pointers are stable so sqlite will
        // not copy the data.  This works as long as the file is
        // mapped, which should be the case always.
        uintptr_t len = cur->len;
        if ((len > 0) && (cur->line[len-1] == '\n')) {
            // Strip the newline
            len--;
        }
        sqlite3_result_text(c, (char*)cur->line, len, SQLITE_STATIC);
        break;
    }
    }
    return SQLITE_OK;
}
static int xRowid(sqlite3_vtab_cursor *pCur, sqlite_int64 *pRowid) {
    // LOG("xRowid\n");
    *pRowid = logparse_cursor(pCur)->rowid;
    return SQLITE_OK;
}

static sqlite3_module Module = {
    0,                  /* iVersion */
    xCreate,            /* xCreate */
    xConnect,           /* xConnect */
    xBestIndex,         /* xBestIndex */
    xDisconnect,        /* xDisconnect */
    xDisconnect,        /* xDestroy */
    xOpen,              /* xOpen - open a cursor */
    xClose,             /* xClose - close a cursor */
    xFilter,            /* xFilter - configure scan constraints */
    xNext,              /* xNext - advance a cursor */
    xEof,               /* xEof - check for end of scan */
    xColumn,            /* xColumn - read data */
    xRowid,             /* xRowid - read data */

    NULL,               /* xUpdate  (NULL is readonly) */
    NULL,               /* xBegin */
    NULL,               /* xSync */
    NULL,               /* xCommit */
    NULL,               /* xRollback */
    NULL,               /* xFindMethod */
    NULL,               /* xRename */
};

void inc(sqlite3_context *c, int argc, sqlite3_value **argv) {
    // LOG("inc\n");
    ASSERT(argc == 1);
    sqlite3_result_int(c, 1 + sqlite3_value_int(argv[0]));
}

int sqlite3_logparse_init(sqlite3 *db, char **err, const sqlite3_api_routines *api) {
    SQLITE_EXTENSION_INIT2(api);
    ASSERT(
        SQLITE_OK ==
        sqlite3_create_function(
            db, "inc", 1,
            SQLITE_UTF8 | SQLITE_DETERMINISTIC, 
            NULL,  // sqlite3_user_data()
            inc,   // xFunc,
            NULL,  // xStep,
            NULL   // xFinal
            ));
    ASSERT(
        SQLITE_OK ==
        sqlite3_create_module(db, "logparse", &Module, 0));
    return SQLITE_OK;
}
