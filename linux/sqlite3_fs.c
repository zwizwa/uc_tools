// File system structure virtual table.  This is ad-hoc, originally
// built map a directory structure of log files to tables,
// representing overarching structure for ilog, logparse virtual
// tables.  The purpose here is very simple: rescan the directory
// structure on every query.  Maybe the ilog needs to change as well
// to create a join across multiple files.


// #define FS_LOG LOG
#ifndef FS_LOG
#define FS_LOG(...)
#endif




//#define MMAP_FILE_LOG LOG

#ifndef VTABLE_NAME
#define VTABLE_NAME "fs"
#endif

#define _GNU_SOURCE         /* See feature_test_macros(7) */
#include <sys/mman.h>
#include <sqlite3ext.h>

#include "macros.h"
#include <stdio.h>
#include <dirent.h>
#include "string.h"

#include "stdint.h"

SQLITE_EXTENSION_INIT1

/* It is set up as an array to facilitate later extension to recursive
   tranversal. */
#define MAX_DEPTH 3

/* The 'base' member contains the base class.  Must be first */
struct fs_table {
    sqlite3_vtab base;
    char *dirname;
    char *column[MAX_DEPTH];
    uintptr_t depth;
};

/* Cursor into an fs is a nested set of dir + current entry for each depth. */
struct dir_cursor {
    DIR *dir;
    struct dirent *entry;
};

struct fs_cursor {
    sqlite3_vtab_cursor base;
    off_t rowid;
    struct dir_cursor path[MAX_DEPTH];
    int depth;

};

static struct fs_cursor *fs_cursor(sqlite3_vtab_cursor *p) {
    return (void*)p;
}
static struct fs_table *fs_table(sqlite3_vtab *p) {
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
    FS_LOG("xConnect\n");
    for(int i=0; i<argc; i++) {
        // arg0 vtable name
        // arg1 ?
        // arg2 table name
        // arg3 first arg
        // arg4 second arg
        // ...
        LOG("arg%d %s\n", i, argv[i]);
    }

    struct fs_table *tab = sqlite3_malloc(sizeof(*tab));
    memset(tab,0,sizeof(*tab));

    ASSERT(argc >= 4);
    tab->dirname = strdup(argv[3]);
    LOG("dir: %s\n", tab->dirname);
    ASSERT(tab->dirname);

    // The rest are column names
    tab->depth = argc - 4;
    ASSERT(tab->depth >= 1);
    ASSERT(tab->depth <= MAX_DEPTH);
    for (int i=0; i<tab->depth; i++) {
        tab->column[i] = strdup(argv[4+i]);
        LOG("%d: %s\n", i, tab->column[i]);
    }

    // Create the SQL that declars the table
    char sql[1024];
    int N = sizeof(sql);
    int n = 0;
    n += snprintf(sql+n, N-n, "CREATE TABLE x(");
    for (int i=0; i<tab->depth; i++) {
        n += snprintf(sql+n, N-n, "%s TEXT, ", tab->column[i], i);
    }
    n += snprintf(sql+n, N-n, "schema HIDDEN)");
    LOG("sql: %s\n", sql);

    // Interpret it
    int rv = sqlite3_declare_vtab(db, sql);
    ASSERT(rv == SQLITE_OK);

    *ppVtab = &tab->base;
    return SQLITE_OK;
}


// create a new instance of a virtual table
static int xCreate(
    sqlite3 *db,
    void *pAux,
    int argc, const char *const*argv,
    sqlite3_vtab **ppVtab,
    char **pzErr){
    FS_LOG("xCreate\n");
    return xConnect(db, pAux, argc, argv, ppVtab, pzErr);
}
static int xDisconnect(sqlite3_vtab *pVtab) {
    return SQLITE_OK;
}

// https://claude.ai/chat/6c19049a-04d1-40d6-bdbd-fcd7bdb0287e
static int xBestIndex(sqlite3_vtab *tab, sqlite3_index_info *p) {
    FS_LOG("xBestIndex\n");
    return SQLITE_OK;
}

static int xClose(sqlite3_vtab_cursor *pCur) {
    FS_LOG("xClose\n");
    sqlite3_free(pCur);
    return SQLITE_OK;
}
// sqlite calls xEof immediately after xNext
static int xEof(sqlite3_vtab_cursor *pCur) {
    FS_LOG("xEof\n");
    struct fs_cursor *cur = fs_cursor(pCur);
    /* When readdir() returns NULL, the handle is closed and the
       pointer is set to NULL to indicate EOF condition. */
    return !cur->path[0].dir;
}
static int xNext(sqlite3_vtab_cursor *pCur) {
    FS_LOG("xNext\n");
    struct fs_cursor *cur = fs_cursor(pCur);
    cur->path[0].entry = readdir(cur->path[0].dir);
    if (!cur->path[0].entry) {
        closedir(cur->path[0].dir);
        cur->path[0].dir = NULL;
    }
    else {
        cur->rowid++;
    }
    return SQLITE_OK;
}
static int xFilter(sqlite3_vtab_cursor *pCur, int idxNum, const char *idxStr,
                   int argc, sqlite3_value **argv) {
    FS_LOG("xFilter (running xNext)\n");
    xNext(pCur);
    return SQLITE_OK;
}

static int xColumn(sqlite3_vtab_cursor *pCur, sqlite3_context *c, int N) {
    FS_LOG("xColumn %d\n", N);
    struct fs_cursor *cur = fs_cursor(pCur);
    switch(N) {
    case 0: {
        ASSERT(cur->path[0].entry);
        sqlite3_result_text(c, cur->path[0].entry->d_name, -1, SQLITE_TRANSIENT);
        break;
    }
    default:
        // Not reached
        SQLITE_ERROR;
    }
    return SQLITE_OK;
}

static int xOpen(sqlite3_vtab *pVTab, sqlite3_vtab_cursor **ppCursor) {
    FS_LOG("xOpen\n");
    struct fs_table *tab = fs_table(pVTab);
    struct fs_cursor *cur = sqlite3_malloc(sizeof(*cur));
    memset(cur,0,sizeof(*cur));
    cur->path[0].dir = opendir(tab->dirname);
    ASSERT(cur->path[0].dir);
    *ppCursor = &cur->base;
    return SQLITE_OK;
}

static int xRowid(sqlite3_vtab_cursor *pCur, sqlite_int64 *pRowid) {
    FS_LOG("xRowid\n");
    *pRowid = fs_cursor(pCur)->rowid;
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




int sqlite3_fs_init(sqlite3 *db, char **err, const sqlite3_api_routines *api) {
    SQLITE_EXTENSION_INIT2(api);
    ASSERT(
        SQLITE_OK ==
        sqlite3_create_module(db, VTABLE_NAME, &Module, 0));
    return SQLITE_OK;
}
