#ifndef MOD_SQLITE3_ILOG
#define MOD_SQLITE3_ILOG

#define _GNU_SOURCE

#include "log_parse_mmf.h"


/* Use mod_Sqlite3_logs.c to implement generic traversal for directory
   tree of ilog files. */

/* The sqlite modules are .so files so it is ok to define a number of
   "global" compile time constructs that take differen values for
   different so modules.  I.e. it is not necessary to use the ns_*h
   name-mangling functor approach. */

typedef struct log_parse_mmf sqlite3_log_t;

#define VTABLE_NAME "logparse"
#define SQLITE3_LOGS_EXT ".bin"

#define sqlite3_log_init sqlite3_logparse_init


#include "mod_sqlite3_logs.c"

void open_log_and_index(struct log_table *t,
                        struct log_cursor *c,
                        const char *log_filename) {
    //LOG("open %s\n", log_filename);
    c->log_filename = strdup(log_filename);
    log_parse_mmf_open(&c->log, log_filename);
    // FIXME: index
}
/* Open ilog file from dir_traverse path state. */
void open_current_log_and_index(struct log_table *t,
                                struct log_cursor *c) {
    char *path = NULL;
    asprintf(&path, "%s/%s/%s/%s",
             t->log_top,
             path_name(c, 0),
             path_name(c, 1),
             path_name(c, 2));
    // LOG("opening ilog %s\n", path);
    open_log_and_index(t, c, path);
}

void close_log_and_index(struct log_cursor *cur) {
    // FIXME: index
    log_parse_mmf_close(&cur->log);
    free((void*)cur->log_filename);
    cur->log_filename = NULL;
    cur->msg_nb = 0;
}

int current_log_eof(struct log_cursor *cur) {
    intptr_t index_size = log_parse_mmf_index_size(&cur->log);
    if (index_size >= 0) {
        /* If the index is active, wind_message() did not use the
           parsing traversal so _eof() won't be valid.  Use the index
           to determine if the current msg_nb is inside the index
           array. */
        return cur->msg_nb >= index_size;
    }
    else {
        return log_parse_mmf_eof(&cur->log);
    }
}

/* Note that the API currently assumes that it is possible to fetch
   cur->msg_nb using random access.

   However, in the simplest no-index case this will only increment, so
   it is enough to have an increment-only iterator for the base log
   file format. */

void wind_message(struct log_cursor *cur) {
    const struct log_parse_index *idx = log_parse_mmf_index(&cur->log);
    if (idx) {
        /* Nothing to do here. current_log_eof() and xColumn() will
           use the index and do not need to do any parsing. */
    }
    else {
        /* If there is no index we can only skip forward using the
           stream parser. */
        ASSERT(cur->log.msg_nb <= cur->msg_nb);
        while(cur->msg_nb > cur->log.msg_nb) {
            log_parse_mmf_next(&cur->log);
            if (current_log_eof(cur)) return;
        }
    }
}

static void declare_vtab(sqlite3 *db) {
    int rv = sqlite3_declare_vtab(
        db,
        "CREATE TABLE x("
        "  ts     INTEGER,"  // 0
        "  bin    INTEGER,"  // 1
        "  line   TEXT,"     // 2
        "  msg_nb INTEGER,"  // 3 -- message number inside file 0=first
        "  file   TEXT,"     // 4 -- path of log file
        "  PRIMARY KEY(file, msg_nb)"
        ") WITHOUT ROWID");
    if (rv != SQLITE_OK) {
        ERROR("declare_vtab error %d\n", rv);
    }
}


static int xColumn(sqlite3_vtab_cursor *pCur, sqlite3_context *c, int N) {
    // LOG("xColumn %d\n", N);
    struct log_cursor *cur = log_cursor(pCur);
    struct log_table *tab = (void*)cur->base.pVtab;
    (void)tab;

    const char *log = log_parse_mmf_log(&cur->log);
    const struct log_parse_index *idx = log_parse_mmf_index(&cur->log);

    if (idx) {
        /* Index can be used. */
        idx += cur->msg_nb;
    }
    else {
        /* The streaming traversal exposes the same index struct as
           the index array */
        ASSERT(cur->msg_nb == cur->log.msg_nb);
        idx = &cur->log.lpi;
    }

    switch(N) {
    case 0: {
        sqlite3_result_int64(c, idx->timestamp);
        break;
    }
    case 1: {
        sqlite3_result_int(c, idx->bin);
        break;
    }
    case 2: {
        const char *line = log + idx->offset + idx->data_offset;
        intptr_t len = idx->data_len;
        if (idx->bin) {
            sqlite3_result_text(c, line, len, SQLITE_TRANSIENT);
        }
        else {
            /* Strip newline if there is one. */
            if ((len > 0) && (line[len-1] == '\n')) len--;
            sqlite3_result_text(c, line, len, SQLITE_TRANSIENT);
        }
        break;
    }
    case 3: {
        sqlite3_result_int(c, cur->msg_nb);
        break;
    }
    case 4: {
        sqlite3_result_text(c, cur->log_filename, -1, SQLITE_TRANSIENT);
        break;
    }
    default:
        sqlite3_result_null(c);
        break;
    }
    return SQLITE_OK;
}

void create_functions(sqlite3 *db) {
}




#endif
