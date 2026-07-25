// Generic 'functor' for adding directory traversal to (memory-mapped)
// abstract file iterators (indexers).

// Refresher on sequencing of the traversal state machine:
//
// xBestIndex  (for query planner)
// xFilter     (start a new query using specific index method)
// xEof
// xColumn     get columns from first row
// xNext       wind to next row
// xEof
// xColumn     get columns from second row


#ifndef MOD_SQLITE3_LOGS
#define MOD_SQLITE3_LOGS

//#define MMAP_FILE_LOG LOG

#ifndef VTABLE_NAME
#error need VTABLE_NAME
#endif

#define _GNU_SOURCE         /* See feature_test_macros(7) */
#include <sys/mman.h>

#include "mmap_file.h"

#include "dir_traverse.h"

#include "sqlite3_vt.h"


#ifndef MOD_SQLITE3_LOG_NB_MMF
#define MOD_SQLITE3_LOG_NB_MMF 1
#endif

/* The 'base' member contains the base class.  Must be first */
struct log_table {
    sqlite3_vtab base;

    /* If we are in single file mode this is passed in as an
       argument to the table creation. */
    const char *log_top;
    int log_depth;
};

/* Cursor into an log is just an integer. */
struct log_cursor {
    sqlite3_vtab_cursor base;

    /* Directory traversal. */
    struct dir_traverse dt;

    /* Indexed message log and path. */
    sqlite3_log_t log;
    const char *log_filename;

    /* Optional flat image file, e.g. for additional index data, or
       bulk logic trace data referenced by other files. */
    struct mmap_file mmf[MOD_SQLITE3_LOG_NB_MMF];

    /* Message sequence number in the current logfile, 0=first. */
    off_t msg_nb;
};

// These need to be provided by the specialized code.
static int xColumn(sqlite3_vtab_cursor *pCur, sqlite3_context *c, int N);
static void declare_vtab(sqlite3 *db);
void open_index(struct log_table *t, struct log_cursor *c,
                const char *log_filename);
void open_log_and_index(struct log_table *t, struct log_cursor *c, const char *log_filename);
void open_current_log_and_index(struct log_table *t,
                                 struct log_cursor *c);
int current_log_eof(struct log_cursor *cur);
void wind_message(struct log_cursor *cur);


static struct log_cursor *log_cursor(sqlite3_vtab_cursor *p) {
    return (void*)p;
}
static struct log_table *log_table(sqlite3_vtab *p) {
    return (void*)p;
}

void close_log_and_index(struct log_cursor *cur);

void close_log_mmf(struct log_cursor *cur) {
    for (int i=0; i<ARRAY_SIZE(cur->mmf); i++) {
        mmap_file_close(&cur->mmf[i]); // Idempotent close
    }
}

const char *path_name(struct log_cursor *cur, unsigned int i) {
    ASSERT(i < cur->dt.end_depth);
    return cur->dt.path[i].entry->d_name;
}
void open_next_log_and_index(struct log_cursor *cur) {
  again:
    close_log_and_index(cur);
    cur->msg_nb = 0;

    dir_traverse_next(&cur->dt);

    if (dir_traverse_end(&cur->dt)) {
        ASSERT(current_log_eof(cur));
        /* Leave the empty sentinel. */
        // LOG("no next log\n");
        return;
    }
    struct log_table *tab = log_table(cur->base.pVtab);
    open_current_log_and_index(tab, cur);

    /* Skip zero size files. */
    if (current_log_eof(cur)) {
        // LOG("empty file\n");
        goto again;
    }
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
    // "log" "temp" "lp" "dev1.bin"

    struct log_table *pNew = sqlite3_malloc(sizeof(*pNew));
    memset(pNew,0,sizeof(*pNew));

    ASSERT(argc >= 4);

    /* Single file or top directory. */
    pNew->log_top = strdup(argv[3]);
    if (argc >= 5) {
        /* Directory tree. */
        pNew->log_depth = atoi(argv[4]);
        ASSERT(pNew->log_depth > 0);
        ASSERT(pNew->log_depth <= DIR_TRAVERSE_MAX_DEPTH);
    }
    else {
        /* Single file. */
        pNew->log_depth = 0;
    }

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

// This will be encoded in idxStr, so needs to be flat and cannot
// contain any pointers.  If the encoding is raw, it cannot contain 0
// characters.
#define DIR_CONSTRAINT_NONE 0xff
struct dir_constraint_index {
    /* If a directory constraing is active, this will point to the
       argv index, while DIR_CONSTRAINT_NONE means constraint is not
       active. */
    int8_t argv_index[MOD_SQLITE3_LOG_NB_MMF];
};


// https://claude.ai/chat/6c19049a-04d1-40d6-bdbd-fcd7bdb0287e
static int xBestIndex(sqlite3_vtab *pVTab, sqlite3_index_info *p) {
#if 0
    LOG("xBestIndex\n");
    db_log_index_info(p);

    /* Constraints on the contents of the log files are a bit too
       advanced atm and maybe not all that useful, but constraints on
       the files will have a large effect, so implement them first.
       Focus on what we can easily do: equality constraints on the
       directory and files, e.g. typically for the logparse is to
       restrict to files in <test_id>/<test_setup>/

    */

    /* So I am going to need to encode the "constraint program" into a
       string.  What do I actually need to know at scan time?  I think
       it is just "do I skip this file or not". */


    for(i=0; i<p->nConstraint; i++){
        /* aConstraint is what we are told by sqlite, and
           aConstraintUsage is what we tell sqlite */
        struct sqlite3_index_constraint *c = &p->aConstraint[i];
        if( c->op==SQLITE_INDEX_CONSTRAINT_EQ && c->usable ){
            /* Note that 0 means "not set". 1 is argv[0], 2 is argv[1]
               etc.. */

            p->aConstraintUsage[i].argvIndex = 1;
            /* Tells sqlite to omit check, i.e. we guarantee that it
               is satisfied fully. */
            p->aConstraintUsage[i].omit = 1;
        }
    }


#endif
    return SQLITE_OK;
}

static int xClose(sqlite3_vtab_cursor *pCur) {
    struct log_cursor *cur = log_cursor(pCur);
    close_log_and_index(cur);
    dir_traverse_close(&cur->dt);
    sqlite3_free(pCur);
    return SQLITE_OK;
}

/* Normalize the current cursor after msg_nb is updated.
   The end result is that:
   - the EOF condition is available for xEof
   - if no EOF, the current message is available for xColumn
*/

void normalize_cursor(struct log_cursor *cur) {
    //LOG("normalize_cursor %d\n", cur->msg_nb);
    wind_message(cur);
    //LOG("normalize_cursor wind message ok\n");
    if (cur->dt.end_depth > 0) {
        /* There is directory traversal.  If we are at the end of the
           current file. */
        if (current_log_eof(cur)) {
            // LOG("normalize_cursor cur eof\n");
            open_next_log_and_index(cur);
            wind_message(cur);
        }
        else {
            // LOG("normalize_cursor can continue\n");
        }
    }
}


// sqlite calls xEof immediately after xNext
static int xEof(sqlite3_vtab_cursor *pCur) {
    struct log_cursor *cur = log_cursor(pCur);
    int eof = current_log_eof(cur);
    // LOG("xEof %d\n", eof);
    return eof;
}
static int xFilter(sqlite3_vtab_cursor *pCur, int idxNum, const char *idxStr,
                   int argc, sqlite3_value **argv) {
    //LOG("xFilter\n");
    struct log_cursor *cur = log_cursor(pCur);
    cur->msg_nb = 0;
    normalize_cursor(cur);
    return SQLITE_OK;
}
static int xNext(sqlite3_vtab_cursor *pCur) {
    struct log_cursor *cur = log_cursor(pCur);
    cur->msg_nb++;
    normalize_cursor(cur);
    // LOG("xNext %d %s\n", cur->msg_nb, cur->log_filename);
    return SQLITE_OK;
}

static void log_cursor_init(struct log_cursor *cur,
                             struct log_table *tab) {
    /* All integer values are initialized to 0. */
    memset(cur,0,sizeof(*cur));

    /* Note that the mmf structs support idempotent close when
       initialized as zero. */

    /* SQLite will set this when xOpen finishes, but we rely on it
       during sync scan so initialize it here. */
    cur->base.pVtab = &tab->base;

    if (tab->log_depth == 0) {
        /* In single-file mode we can open everything already.  In
           multi-file mode the filename will need to come from the
           xBestIndex data. */
        // LOG("open file %d\n", tab->log_top);
        open_log_and_index(tab, cur, tab->log_top);
    }
    else {
        /* In multi-file mode we initialize the directory
           traversal. */
        // LOG("open tree %d\n", tab->log_top);
        dir_traverse_init(&cur->dt, tab->log_top, tab->log_depth);
        cur->dt.ext = SQLITE3_LOGS_EXT;
        /* Open the first file or load an empty sentinel. */
        open_next_log_and_index(cur);
    }

}

static int xOpen(sqlite3_vtab *pVTab, sqlite3_vtab_cursor **ppCursor) {
    //LOG("xOpen\n");
    struct log_table *tab = log_table(pVTab);
    struct log_cursor *cur = sqlite3_malloc(sizeof(*cur));

    log_cursor_init(cur,tab);

    *ppCursor = &cur->base;
    return SQLITE_OK;
}

// FIXME: This only works for single file.  Add a composite primary key.
static int xRowid(sqlite3_vtab_cursor *pCur, sqlite_int64 *pRowid) {
    // LOG("xRowid\n");
    *pRowid = log_cursor(pCur)->msg_nb;
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

void create_functions(sqlite3 *db);

int sqlite3_log_init(sqlite3 *db, char **err, const sqlite3_api_routines *api) {
    SQLITE_EXTENSION_INIT2(api);
    ASSERT(
        SQLITE_OK ==
        sqlite3_create_module(db, VTABLE_NAME, &Module, 0));
    create_functions(db);
    return SQLITE_OK;
}


#endif
