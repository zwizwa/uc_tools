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
};

struct logparse_cursor {
    sqlite3_vtab_cursor base;
    struct log_parse lp;
    uint32_t rowid;
    uint8_t eof:1;
};

static struct logparse_cursor *logparse_cursor(sqlite3_vtab_cursor *p) {
    return (void*)p;
}
static struct logparse_table *logparse_table(sqlite3_vtab *p) {
    return (void*)p;
}

#if 0
static uintptr_t mmap_file_offset(struct logparse_cursor *cur) {
    struct logparse_table *tab = logparse_table(cur->base.pVtab);
    const uint8_t *start = tab->file.buf;
    return cur->lp.in_mark - start;
}
#endif


static log_parse_status_t ts_line_cb(
    struct log_parse *s, uint32_t ts,
    const uint8_t *line, uintptr_t len) {
    LOG("ts_line_cb %08x\n", ts);
    return LOG_PARSE_STATUS_YIELD;
}
static log_parse_status_t ts_bin_cb(
    struct log_parse *s, uint32_t ts,
    const uint8_t *line, uintptr_t len) {
    LOG("ts_bin_cb %08x\n", ts);
    return LOG_PARSE_STATUS_YIELD;
}
static struct log_parse_cbs cbs = {
    .line    = ts_line_cb,
    .ts_line = ts_line_cb,
    .ts_bin  = ts_bin_cb,
};


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
        "id INTEGER PRIMARY KEY,"
        "val INTEGER,"
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

static int xOpen(sqlite3_vtab *pVTab, sqlite3_vtab_cursor **ppCursor) {
    //LOG("xOpen\n");
    struct logparse_table *tab = logparse_table(pVTab);
    struct logparse_cursor *cur = sqlite3_malloc(sizeof(*cur));
    memset(cur,0,sizeof(*cur));
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
    lp->cb = &cbs;

    *ppCursor = &cur->base;
    return SQLITE_OK;
}
static int xClose(sqlite3_vtab_cursor *pCur) {
    //LOG("xClose\n");
    sqlite3_free(pCur);
    return SQLITE_OK;
}
static int xEof(sqlite3_vtab_cursor *pCur) {
    //return logparse_cursor(pCur)->rowid > 10;
    int eof = logparse_cursor(pCur)->eof;
    LOG("xEof %d\n", eof);
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
    LOG("xNext %d\n", s);
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
static int xColumn(sqlite3_vtab_cursor *pCur, sqlite3_context *db, int N) {
    //LOG("xColumn %d\n", N);
    ASSERT(N < 2);
    int id = logparse_cursor(pCur)->rowid;
    if (N == 0) {
        sqlite3_result_int(db, id);
    }
    else if (N == 1) {
        sqlite3_result_int(db, id*id);
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

int sqlite3_logparse_init(sqlite3 *db, char **err, const sqlite3_api_routines *api) {
    SQLITE_EXTENSION_INIT2(api);
    return sqlite3_create_module(db, "logparse", &Module, 0);
}
