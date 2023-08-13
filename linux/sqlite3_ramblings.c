// Wrap a ramblings log as an sqlite3 table.

// See examples:
// https://github.com/ralight/sqlite3-pcre/blob/master/pcre.c
// https://www.sqlite.org/loadext.html -> 4.1
// https://www.sqlite.org/carray.html
// https://www.sqlite.org/src/file/ext/misc
// https://www.sqlite.org/vtab.html#tabfunc2
// https://www.sqlite.org/vtablist.html

// SQL side looks like:

// The extension module file is logparse.so
// sqlite> .load ./ramblings

// CREATE VIRTUAL TABLE r USING ramblings("exo.txt");

#define _GNU_SOURCE         /* See feature_test_macros(7) */
#include <sys/mman.h>

#include <sqlite3ext.h>

#include "mmap_file.h"

SQLITE_EXTENSION_INIT1

/* The 'base' member contains the base class.  Must be first */
struct ramblings_table {
    sqlite3_vtab base;
    struct mmap_file file;
};

struct ramblings_cursor {
    sqlite3_vtab_cursor base;

    uint32_t rowid;

    const char *buf; uintptr_t buf_len; uintptr_t buf_offset;

    /* Index into mmapped data. */
    const char *date;  uintptr_t date_len;
    const char *title; uintptr_t title_len;

    /* xNext calling log_parse_continue() can determine end-of-file
       condition, which is saved ere and picked up by xEof */
    uint8_t eof:1;

};

static struct ramblings_cursor *ramblings_cursor(sqlite3_vtab_cursor *p) {
    return (void*)p;
}
static struct ramblings_table *ramblings_table(sqlite3_vtab *p) {
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
        LOG("arg%d %s\n", i, argv[i]);
    }
    // "ramblings" "temp" "lp" "dev1.bin"

    struct ramblings_table *pNew = sqlite3_malloc(sizeof(*pNew));
    memset(pNew,0,sizeof(*pNew));

    // This contains syntax, e.g. for strings the quotes are included.
    // We just assume quotes are there, and that filenames do not
    // contain quotes. FIXME: Is there a reusable parser for this?
    ASSERT(argc >= 4);
    int n = strlen(argv[3])-2+1;
    char filename[n];
    memcpy(filename, argv[3]+1, n-1);
    filename[n-1] = 0;
    LOG("ramblings %s\n", filename);

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
    int eof = ramblings_cursor(pCur)->eof;
    //LOG("xEof %d\n", eof);
    return eof;
}
static int xFilter(sqlite3_vtab_cursor *pCur, int idxNum, const char *idxStr,
                   int argc, sqlite3_value **argv) {
    //LOG("xFilter\n");
    return SQLITE_OK;
}


static int xNext(sqlite3_vtab_cursor *pCur) {
    LOG("xNext\n");
    struct ramblings_cursor *cur = ramblings_cursor(pCur);
    const char *c     = cur->buf + cur->buf_offset - 1;
    const char *c_max = cur->buf + cur->buf_len;

#define MATCH(i,char) { \
    if (unlikely(c >= c_max)) goto eof; \
    if (likely(c[i] != char)) goto next; \
}
  next:
    c++;
    MATCH(0, '\n');
    MATCH(1, '\n');
    MATCH(2, 'E');
    MATCH(3, 'n');
    MATCH(4, 't');
    MATCH(5, 'r');
    MATCH(6, 'y');
    MATCH(7, ':');
#undef MATCH

    c+=2;

    /* c points points at 'Entry:' now.  This is a good place to start
       scanning next (FIXME). */
    cur->buf_offset = c - cur->buf;

    // FIXME
    cur->date  = c; cur->date_len = 1;
    cur->title = c; cur->title_len = 20;

    return SQLITE_OK;


  eof:
    cur->eof = 1;
    return SQLITE_OK;
}

static void ramblings_cursor_init(struct ramblings_cursor *cur,
                                  struct ramblings_table *tab) {
    /* All integer values are initialized to 0. */
    memset(cur,0,sizeof(*cur));

    /* SQLite will set this when xOpen finishes.  Was necessary in
       template code.  Still necessary? */
    cur->base.pVtab = &tab->base;

    cur->buf     = tab->file.buf;
    cur->buf_len = tab->file.size;

}

static int xOpen(sqlite3_vtab *pVTab, sqlite3_vtab_cursor **ppCursor) {
    //LOG("xOpen\n");
    struct ramblings_table *tab = ramblings_table(pVTab);
    struct ramblings_cursor *cur = sqlite3_malloc(sizeof(*cur));

    ramblings_cursor_init(cur,tab);

    /* SQLITE will call xEof xColumn then xNext xEof xColumn etc...
       So we need to call it once here to get started. */
    xNext(&cur->base);

    *ppCursor = &cur->base;
    return SQLITE_OK;
}

/* https://sqlite.org/c3ref/result_blob.html

   The sqlite3_result_text() interfaces set the return value of the
   application-defined function to be a text string which is
   represented as UTF-8.

   If the 3rd parameter to the sqlite3_result_text* interfaces is
   non-negative, then as many bytes (not characters) of the text
   pointed to by the 2nd parameter are taken as the
   application-defined function result.

   If the 4th parameter to the sqlite3_result_text* interfaces or to
   sqlite3_result_blob is the special constant SQLITE_STATIC, then
   SQLite assumes that the text or BLOB result is in constant space
   and does not copy the content of the parameter nor call a
   destructor on the content when it has finished using that result.
*/

static int xColumn(sqlite3_vtab_cursor *pCur, sqlite3_context *c, int N) {
    // LOG("xColumn %d\n", N);
    struct ramblings_cursor *cur = ramblings_cursor(pCur);
    switch(N) {
    case 0: {
        // Timestamp.
        sqlite3_result_text(c, cur->date, cur->date_len, SQLITE_STATIC);
        break;
    }
    case 1: {
        // Title
        sqlite3_result_text(c, cur->title, cur->title_len, SQLITE_STATIC);
        break;
    }
    }
    return SQLITE_OK;
}
static int xRowid(sqlite3_vtab_cursor *pCur, sqlite_int64 *pRowid) {
    // LOG("xRowid\n");
    *pRowid = ramblings_cursor(pCur)->rowid;
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

int sqlite3_ramblings_init(sqlite3 *db, char **err, const sqlite3_api_routines *api) {
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
        sqlite3_create_module(db, "ramblings", &Module, 0));
    return SQLITE_OK;
}
