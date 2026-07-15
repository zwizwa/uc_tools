#ifndef MOD_SQLITE3
#define MOD_SQLITE3

/* Convenience routines for the sqlite-as-daemon-state pattern used in
   etc-net-daemon and unbrickable server, router. */

#ifndef SQL_LOG
#define SQL_LOG(...)
#endif

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
void db_load_extensions(const char *const *module) {
    sqlite3_enable_load_extension(db, 1);
    if (!module) return;
    for (int i=0; module[i]; i++) {
        char *errmsg = NULL;
        int rc = sqlite3_load_extension(db, module[i], NULL, &errmsg);
        if (rc != SQLITE_OK) {
            ERROR("load_extension failed: %s\n", errmsg);
            sqlite3_free(errmsg);
        }
        else {
            LOG("loaded %s\n", module[i]);
        }
    }
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
        SQL_LOG("prepare: %p %s\n", ps, q);
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

//int nb_transactions = 0;
void db_begin_transaction(void) {
    //ASSERT(db);
    //nb_transactions++;
    //LOG("BEGIN TRANSACTION %d\n", nb_transactions);
    static sqlite3_stmt *s; stmt(&s, "BEGIN TRANSACTION");
    int rv = sqlite3_step(s);
    sqlite_assert_eq(rv, SQLITE_DONE);
}
void db_end_transaction(void) {
    //LOG("END TRANSACTION %d\n", nb_transactions);
    //nb_transactions--;
    static sqlite3_stmt *s; stmt(&s, "END TRANSACTION");
    int rv = sqlite3_step(s);
    sqlite_assert_eq(rv, SQLITE_DONE);
}


#endif
