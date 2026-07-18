#ifndef MOD_SQLITE3_ILOG
#define MOD_SQLITE3_ILOG

#include "ilog.h"


/* Use mod_Sqlite3_logs.c to implement generic traversal for directory
   tree of ilog files. */

/* The sqlite modules are .so files so it is ok to define a number of
   "global" compile time constructs that take differen values for
   different so modules.  I.e. it is not necessary to use the ns_*h
   name-mangling functor approach. */

typedef struct ilog_read sqlite3_log_t;

#ifndef VTABLE_NAME
#define VTABLE_NAME "ilog"
#endif

#define SQLITE3_LOGS_EXT ".ilog"

#include "mod_sqlite3_logs.c"

void open_log_and_index(struct log_table *t,
                        struct log_cursor *c,
                        const char *log_filename) {
    c->log_filename = strdup(log_filename);
    ilog_open_read(&c->log, log_filename);
    open_index(t, c, log_filename);
}

void close_log_and_index(struct log_cursor *cur) {
    close_log_mmf(cur);
    ilog_read_close(&cur->log);
    free((void*)cur->log_filename);
    cur->log_filename = NULL;
}

int current_log_eof(struct log_cursor *cur) {
    return (cur->msg_nb >= cur->log.ilog.nb_messages);
}

void wind_message(struct log_cursor *cur) {
    ilog_wind_message(&cur->log, cur->msg_nb);
}

#endif
