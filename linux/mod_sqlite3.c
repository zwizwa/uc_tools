#ifndef MOD_SQLITE3
#define MOD_SQLITE3

/* Convenience routines for the sqlite-as-daemon-state pattern used in
   etc-net-daemon and unbrickable server, router. */

/* DATABASE */
#include "macros.h"
#include <sqlite3.h>
sqlite3 *db = NULL;
void sqlite_assert_eq(int rv, int rv_expected) {
    if (rv != rv_expected) {
        const char *msg = sqlite3_errmsg(db);
        LOG("sql_error %d %s\n", rv, msg);
        exit(1);
    }
}
void sqlite_assert(int rv) {
    sqlite_assert_eq(rv, SQLITE_OK);
}

#define ASSERT_SQLITE(cmd) sqlite_assert(cmd)
void db_open(const char *db_file) {
    ASSERT_SQLITE(sqlite3_open(db_file, &db));
    /* For emu it is not necessary to attach a tmp database to reduce
       write stress due to ephemeral (connection) data. */
    // db_attach_tmp(DB_TMP_DEFAULT);

    // https://www.sqlite.org/wal.html
}
/* DB STATEMENTS */

struct stmt_list;
struct stmt_list {
    sqlite3_stmt *s;
    struct stmt_list *next;
};
struct stmt_list *stmt_list = NULL;

sqlite3_stmt *stmt(sqlite3_stmt **ps, const char *q) {
    struct sqlite3_stmt *tmp_ps = NULL;
    if (ps == NULL) {
        ps = &tmp_ps;
    }
    if (*ps == NULL) {
        LOG("prepare: %p %s\n", ps, q);
        ASSERT_SQLITE(sqlite3_prepare_v2(db, q, strlen(q), ps, NULL));
        /* Keep track of list to be able to properly shut down. */
        struct stmt_list *l = malloc(sizeof(*l));
        ASSERT(l);
        l->next = stmt_list;
        l->s = *ps;
        stmt_list = l;
    }
    struct sqlite3_stmt *s = *ps;
    ASSERT_SQLITE(sqlite3_reset(s));
    ASSERT_SQLITE(sqlite3_clear_bindings(s));
    return s;
}
void stmts_finalize(void) {
    struct stmt_list *l = stmt_list;
    while(l) {
        struct stmt_list *l0 = l;
        l = l->next;
        sqlite3_finalize(l0->s);
        free(l0);
    }
}

void db_attach_tmp(const char *db_file) {
    sqlite3_stmt *s = stmt(NULL /* Not stored */, "ATTACH DATABASE ? AS tmp");
    ASSERT_SQLITE(sqlite3_bind_text(s, 1, db_file, strlen(db_file), NULL));
    int rv = sqlite3_step(s);
    sqlite_assert_eq(rv, SQLITE_DONE);
    sqlite3_finalize(s);  /* Delete because not stored. */
}

// ephemeral statements, not cached
void db_sql0(const char *sql) {
    sqlite3_exec(db, sql, NULL, NULL, NULL);
}


void db_begin_transaction(void) {
    static sqlite3_stmt *s; stmt(&s, "BEGIN TRANSACTION");
    int rv = sqlite3_step(s);
    sqlite_assert_eq(rv, SQLITE_DONE);
}
void db_end_transaction(void) {
    static sqlite3_stmt *s; stmt(&s, "END TRANSACTION");
    int rv = sqlite3_step(s);
    sqlite_assert_eq(rv, SQLITE_DONE);
}





#endif
