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
    const char *filename;
};

struct entry {
    const char *title; uintptr_t title_len;
    const char *date;  uintptr_t date_len;
    const char *body;  uintptr_t body_len;
};

struct ramblings_cursor {
    sqlite3_vtab_cursor base;

    uint32_t rowid;

    const char *buf;
    uintptr_t buf_len;

    /* The current entry is bounded by 2 indices: start of entry and
       start of the next entry or end-of-buffer.

       The header text before the first entry is ignored. */

    uintptr_t entry_start;
    uintptr_t entry_endx;

    /* Parse of the current entry. */
    struct entry entry;

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
        // LOG("arg%d %s\n", i, argv[i]);
    }
    // "ramblings" "temp" "lp" "dev1.bin"

    struct ramblings_table *pNew = sqlite3_malloc(sizeof(*pNew));
    memset(pNew,0,sizeof(*pNew));

    // This contains syntax, e.g. for strings the quotes are included.
    // We just assume quotes are there, and that filenames do not
    // contain quotes. FIXME: Is there a reusable parser for this?
    ASSERT(argc >= 4);
    int n = strlen(argv[3])-2+1;
    char *filename = malloc(n);
    pNew->filename = filename;
    memcpy(filename, argv[3]+1, n-1);
    filename[n-1] = 0;
    LOG("ramblings %s\n", filename);

    mmap_file_open_ro(&pNew->file, filename);
    ASSERT(pNew->file.buf);

    *ppVtab = &pNew->base;
    int rv = sqlite3_declare_vtab(
        db,
        "CREATE TABLE x("
        "title TEXT,"
        "date TEXT,"
        "body TEXT,"
        "file TEXT,"
        "start INTEGER,"
        "endx  INTEGER,"
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
    struct ramblings_cursor *cur = ramblings_cursor(pCur);
    int eof = cur->entry_start == cur->buf_len;
    //LOG("xEof %d\n", eof);
    return eof;
}
static int xFilter(sqlite3_vtab_cursor *pCur, int idxNum, const char *idxStr,
                   int argc, sqlite3_value **argv) {
    //LOG("xFilter\n");
    return SQLITE_OK;
}


void scan_next(struct ramblings_cursor *cur) {
    const char *c     = cur->buf + cur->entry_endx;
    const char *c_max = cur->buf + cur->buf_len;
    goto next0;

#define MATCH(i,char) { \
    if (unlikely(c >= c_max)) goto eof; \
    if (likely(c[i] != char)) goto next; \
}
  next:
    c++;
  next0:
    MATCH(0, '\n');
    MATCH(1, '\n');
    MATCH(2, 'E');
    MATCH(3, 'n');
    MATCH(4, 't');
    MATCH(5, 'r');
    MATCH(6, 'y');
    MATCH(7, ':');
    MATCH(8, ' ');
#undef MATCH

    c+=2;

    /* c points points at 'Entry:' now.  This is a good place to start
       scanning next (FIXME). */
    cur->entry_endx = c - cur->buf;
    return;

  eof:
    cur->entry_endx = cur->buf_len;
}

void parse_entry(struct ramblings_cursor *cur) {
    const char *c      = cur->buf + cur->entry_start;
    const char *c_endx = cur->buf + cur->entry_endx;
    const char *line;
  next_line:
    line = c;
  next_char:
    if(c >= c_endx) goto done;
    if(*c == '\n') {
        int n = c - line;
        if (n == 0) {
            /* End of header. */
            c++;
            cur->entry.body = c;
            cur->entry.body_len = c_endx - c;
            goto done;
        }
        /* Non-empty line. */
        if ((n >= 7) && (0 == memcmp(line, "Entry: ", 7))) {
            cur->entry.title = line + 7;
            cur->entry.title_len = n - 7;
        }
        else if ((n >= 6) && (0 == memcmp(line, "Date: ", 6))) {
            cur->entry.date = line + 6;
            cur->entry.date_len = n - 6;
        }
        c++;
        goto next_line;
    }
    c++;
    goto next_char;

  done:
    return;
}


static int xNext(sqlite3_vtab_cursor *pCur) {
    // LOG("xNext\n");
    struct ramblings_cursor *cur = ramblings_cursor(pCur);
    cur->entry_start = cur->entry_endx;
    scan_next(cur);
    if (cur->entry_start != cur->buf_len) {
        parse_entry(cur);
    }
    else {
        memset(&cur->entry, 0, sizeof(cur->entry));
    }
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
       We need to call these two in order to set up the correct
       initial cursor state. */
    scan_next(cur);
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
    struct ramblings_table *tab = (void*)cur->base.pVtab;

    switch(N) {
    case 0: {
        sqlite3_result_text(c, cur->entry.title, cur->entry.title_len, SQLITE_STATIC);
        break;
    }
    case 1: {
#if 0
        sqlite3_result_text(c, cur->entry.date, cur->entry.date_len, SQLITE_STATIC);
#else
        /* Convert it to something sqlite can understand. */
        char buf[] = "YYYY-MM-DD HH:MM:SS";
        const char *d = cur->entry.date;
        int n = cur->entry.date_len;
        if (n == 0) break; // FIXME eof issue

        /* This is rigid, but prob will never change.  Can't see how
           to do this in a cleaner way without being much less
           efficient. */
        memcpy(buf,      d + n - 4, 4);
        memcpy(buf +  8, d + 8,     2);
        memcpy(buf + 11, d + 11,    8);
        if (buf[8] == ' ') { buf[8] = '0'; };
        const char *m = d + 4;
        char *m1 = buf + 5;
        if      (!memcmp(m,"Jan",3)) { memcpy(m1, "01", 2); }
        else if (!memcmp(m,"Feb",3)) { memcpy(m1, "02", 2); }
        else if (!memcmp(m,"Mar",3)) { memcpy(m1, "03", 2); }
        else if (!memcmp(m,"Apr",3)) { memcpy(m1, "04", 2); }
        else if (!memcmp(m,"May",3)) { memcpy(m1, "05", 2); }
        else if (!memcmp(m,"Jun",3)) { memcpy(m1, "06", 2); }
        else if (!memcmp(m,"Jul",3)) { memcpy(m1, "07", 2); }
        else if (!memcmp(m,"Aug",3)) { memcpy(m1, "08", 2); }
        else if (!memcmp(m,"Sep",3)) { memcpy(m1, "09", 2); }
        else if (!memcmp(m,"Oct",3)) { memcpy(m1, "10", 2); }
        else if (!memcmp(m,"Nov",3)) { memcpy(m1, "11", 2); }
        else if (!memcmp(m,"Dec",3)) { memcpy(m1, "12", 2); }
        sqlite3_result_text(c, buf, 19, SQLITE_TRANSIENT);
#endif
        break;
    }
    case 2: {
        sqlite3_result_text(c, cur->entry.body, cur->entry.body_len, SQLITE_STATIC);
        break;
    }
    case 3: {
        sqlite3_result_text(c, tab->filename, -1, SQLITE_STATIC);
        break;
    }
    case 4: {
        sqlite3_result_int(c, cur->entry_start);
        break;
    }
    case 5: {
        sqlite3_result_int(c, cur->entry_endx);
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
    //LOG("extension init\n");
    SQLITE_EXTENSION_INIT2(api);
    //LOG("create function\n");
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
    //LOG("create module\n");
    ASSERT(
        SQLITE_OK ==
        sqlite3_create_module(db, "ramblings", &Module, 0));
    return SQLITE_OK;
}
