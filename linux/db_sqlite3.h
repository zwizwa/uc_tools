/* Stateless convenience routines on top of slite3.h */

#ifndef DB_SQLITE3_H
#define DB_SQLITE3_H

#ifndef SQL_LOG
#define SQL_LOG(...)
#endif

#include "macros.h"
#include <sqlite3.h>

static inline void db_assert_eq(sqlite3 *db, int rv, int rv_expected) {
    if (rv != rv_expected) {
        const char *msg = sqlite3_errmsg(db);
        LOG("sql_error %d %s\n", rv, msg);
        exit(1);
    }
}
static inline void db_assert(sqlite3 *db, int rv) {
    db_assert_eq(db, rv, SQLITE_OK);
}

/* This only works inside functions that have 'db' bound,
   or inside 'mod_sqlite3.c' that has it bound as a global singleton. */
#define ASSERT_SQLITE(cmd) db_assert(db, cmd)




/* DB STATEMENTS */

struct stmt_list;
struct stmt_list {
    sqlite3_stmt *s;
    struct stmt_list *next;
};


static inline sqlite3_stmt *db_stmt(struct sqlite3 *db,
                                    struct stmt_list **stmt_list,
                                    sqlite3_stmt **ps, const char *q) {

    struct sqlite3_stmt *tmp_ps = NULL;
    if (ps == NULL) {
        ps = &tmp_ps;
    }
    if (*ps == NULL) {
        SQL_LOG("prepare: %p %s\n", ps, q);
        ASSERT_SQLITE(sqlite3_prepare_v2(db, q, strlen(q), ps, NULL));
        /* Keep track of list to be able to properly shut down. */
        struct stmt_list *l = malloc(sizeof(*l));
        ASSERT(l);
        l->next = *stmt_list;
        l->s = *ps;
        *stmt_list = l;
    }
    struct sqlite3_stmt *s = *ps;
    ASSERT_SQLITE(sqlite3_reset(s));
    ASSERT_SQLITE(sqlite3_clear_bindings(s));
    return s;
}

static inline void db_stmts_finalize(struct stmt_list *l) {
    while(l) {
        struct stmt_list *l0 = l;
        l = l->next;
        sqlite3_finalize(l0->s);
        free(l0);
    }
}


#endif
