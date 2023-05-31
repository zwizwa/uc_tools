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


#include "log_parse.h"
#include <sqlite3ext.h>

SQLITE_EXTENSION_INIT1

/* The 'base' member contains the base class.  Must be first */
struct logparse_table {
    sqlite3_vtab base;
};

struct logparse_cursor {
    sqlite3_vtab_cursor base;
    int id;
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
    LOG("xConnect\n");
    struct logparse_table *pNew = sqlite3_malloc(sizeof(*pNew));
    memset(pNew,0,sizeof(*pNew));
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
    struct logparse_cursor *pNew = sqlite3_malloc(sizeof(*pNew));
    memset(pNew,0,sizeof(*pNew));
    *ppCursor = &pNew->base;
    return SQLITE_OK;
}
static int xClose(sqlite3_vtab_cursor *pCur) {
    //LOG("xClose\n");
    sqlite3_free(pCur);
    return SQLITE_OK;
}
static struct logparse_cursor *lpcur(sqlite3_vtab_cursor *pCur) {
    return (void*)pCur;
}
static int xEof(sqlite3_vtab_cursor *pCur) {
    //LOG("xEof\n");
    return lpcur(pCur)->id > 10;
}
static int xFilter(sqlite3_vtab_cursor *pCur, int idxNum, const char *idxStr,
                   int argc, sqlite3_value **argv) {
    //LOG("xFilter\n");
    return SQLITE_OK;
}
static int xNext(sqlite3_vtab_cursor *pCur) {
    //LOG("xNext\n");
    lpcur(pCur)->id++;
    return SQLITE_OK;
}
static int xColumn(sqlite3_vtab_cursor *pCur, sqlite3_context *db, int N) {
    //LOG("xColumn %d\n", N);
    ASSERT(N < 2);
    int id = lpcur(pCur)->id;
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
    *pRowid = lpcur(pCur)->id;
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
