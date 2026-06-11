// See sqlite3_log.c

// This code contains glue code that is shared across all ilog
// formats, where column and index can be implemented based on more
// knowledge of the internal format.

#ifndef MOD_SQLITE3_ILOG
#define MOD_SQLITE3_ILOG

#define _GNU_SOURCE         /* See feature_test_macros(7) */
#include <sys/mman.h>

#include "ilog.h"
#include <sqlite3ext.h>


SQLITE_EXTENSION_INIT1

/* The 'base' member contains the base class.  Must be first */
struct ilog_table {
    sqlite3_vtab base;
    struct ilog_read ilog;
};

/* Cursor into an ilog is just an integer. */
struct ilog_cursor {
    sqlite3_vtab_cursor base;
    struct ilog_read *ilog;
    off_t rowid;
    int idxNum;
    int eof; // e.g. idxNum == 1 uses this
};

// These need to be provided by the specialized code.
static int xColumn(sqlite3_vtab_cursor *pCur, sqlite3_context *c, int N);
static void declare_vtab(sqlite3 *db);


static struct ilog_cursor *ilog_cursor(sqlite3_vtab_cursor *p) {
    return (void*)p;
}
static struct ilog_table *ilog_table(sqlite3_vtab *p) {
    return (void*)p;
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
        // LOG("arg%d %s\n", i, argv[i]);
    }
    // "ilog" "temp" "lp" "dev1.bin"

    struct ilog_table *pNew = sqlite3_malloc(sizeof(*pNew));
    memset(pNew,0,sizeof(*pNew));

    // This contains syntax, e.g. for strings the quotes are included.
    // We just assume quotes are there, and that filenames do not
    // contain quotes. FIXME: Is there a reusable parser for this?
    ASSERT(argc >= 4);
    int n = strlen(argv[3])-2+1;
    char filename[n];
    memcpy(filename, argv[3]+1, n-1);
    filename[n-1] = 0;
    LOG("ilog %s\n", filename);

    ilog_open_read(&pNew->ilog, filename);

    // The specialized module defines the table layout.
    declare_vtab(db);

    *ppVtab = &pNew->base;
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

// https://claude.ai/chat/6c19049a-04d1-40d6-bdbd-fcd7bdb0287e
static int xBestIndex(sqlite3_vtab *tab, sqlite3_index_info *p) {
#if 1
    for (int i = 0; i < p->nConstraint; i++) {
        const struct sqlite3_index_constraint *c = &p->aConstraint[i];
        if (c->usable && c->iColumn == -1 && c->op == SQLITE_INDEX_CONSTRAINT_EQ) {
            p->aConstraintUsage[i].argvIndex = 1;
            p->aConstraintUsage[i].omit = 1;
            p->idxNum = 1;                  /* "rowid point lookup" strategy */
            p->estimatedCost = 1.0;
            p->estimatedRows = 1;           /* requires SQLite >= 3.8.2 */
            return SQLITE_OK;
        }
    }
    p->idxNum = 0;                      /* full scan */
    p->estimatedCost = 1e6;             /* scale to your row count */
    //LOG("xBestIndex\n");
#endif
    return SQLITE_OK;
}

static int xClose(sqlite3_vtab_cursor *pCur) {
    //LOG("xClose\n");
    sqlite3_free(pCur);
    return SQLITE_OK;
}
// sqlite calls xEof immediately after xNext
static int xEof(sqlite3_vtab_cursor *pCur) {
    struct ilog_cursor *cur = ilog_cursor(pCur);
    if (cur->idxNum == 1) {
        return cur->eof;
    }
    int eof = cur->rowid >= cur->ilog->ilog.nb_messages;
    //LOG("xEof %d\n", eof);
    return eof;
}
static int xFilter(sqlite3_vtab_cursor *pCur, int idxNum, const char *idxStr,
                   int argc, sqlite3_value **argv) {
    //LOG("xFilter\n");
    struct ilog_cursor *cur = ilog_cursor(pCur);
    if (idxNum == 1) {
        /* Point loopup: WHERE rowid = ? -- argv[0] */
        ASSERT(argc == 1);
        cur->rowid = sqlite3_value_int64(argv[0]);
    }
    cur->idxNum = idxNum;
    return SQLITE_OK;
}
static int xNext(sqlite3_vtab_cursor *pCur) {
    //LOG("xNext\n");
    struct ilog_cursor *cur = ilog_cursor(pCur);
    if (cur->idxNum == 1) {
        cur->eof = 1;
        return SQLITE_OK;
    }
    cur->rowid++;
    return SQLITE_OK;
}

static void ilog_cursor_init(struct ilog_cursor *cur,
                             struct ilog_table *tab) {
    /* All integer values are initialized to 0. */
    memset(cur,0,sizeof(*cur));

    /* SQLite will set this when xOpen finishes, but we rely on it
       during sync scan so initialize it here. */
    cur->base.pVtab = &tab->base;

    /* Multiple cursors can share the same table.  The ilog_read
       structure only describes the files and the memory mappings and
       has no other state, so we can just link it here. */
    cur->ilog = &tab->ilog;

}

static int xOpen(sqlite3_vtab *pVTab, sqlite3_vtab_cursor **ppCursor) {
    //LOG("xOpen\n");
    struct ilog_table *tab = ilog_table(pVTab);
    struct ilog_cursor *cur = sqlite3_malloc(sizeof(*cur));

    ilog_cursor_init(cur,tab);

    *ppCursor = &cur->base;
    return SQLITE_OK;
}

static int xRowid(sqlite3_vtab_cursor *pCur, sqlite_int64 *pRowid) {
    // LOG("xRowid\n");
    *pRowid = ilog_cursor(pCur)->rowid;
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

int sqlite3_ilog_init(sqlite3 *db, char **err, const sqlite3_api_routines *api) {
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
        sqlite3_create_module(db, "ilog", &Module, 0));
    return SQLITE_OK;
}


#endif
