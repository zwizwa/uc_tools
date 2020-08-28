// see https://sqlite.org/src/file/ext/misc/csv.c
#include <sqlite3ext.h>
SQLITE_EXTENSION_INIT1

typedef struct RamblingsTable {
    sqlite3_vtab base;              /* Base class.  Must be first */
} RamblingsTable;

static int xConnect(
    sqlite3 *db,
    void *pAux,
    int argc, const char *const*argv,
    sqlite3_vtab **ppVtab,
    char **pzErr){
    RamblingsTable *pNew = sqlite3_malloc(sizeof(*pNew));
    memset(pNew,0,sizeof(*pNew));
    *ppVtab = (sqlite3_vtab*)pNew;
    
    return SQLITE_OK;
}
static int xCreate(
    sqlite3 *db,
    void *pAux,
    int argc, const char *const*argv,
    sqlite3_vtab **ppVtab,
    char **pzErr){
    return xConnect(db, pAux, argc, argv, ppVtab, pzErr);
}
static int xDisconnect(sqlite3_vtab *pVtab) {
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
    0,                  /* xUpdate */
    0,                  /* xBegin */
    0,                  /* xSync */
    0,                  /* xCommit */
    0,                  /* xRollback */
    0,                  /* xFindMethod */
    0,                  /* xRename */
};

int sqlite3_rambings_init(sqlite3 *db, char **pzErrMsg, const sqlite3_api_routines *pApi) {
    SQLITE_EXTENSION_INIT2(pApi);
    return sqlite3_create_modules(db, "ramblings", &Module, 0);
}
