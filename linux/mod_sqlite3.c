#ifndef MOD_SQLITE3
#define MOD_SQLITE3

/* The stateless part of the code is being migrated to db_sqlite3.h */
#include "db_sqlite3.h"

/* Convenience routines for the sqlite-as-daemon-state pattern used in
   etc-net-daemon and unbrickable server, router.  These assume there
   is only one database connection in the application binary which
   allows global variables to be used for database and statement list. */

sqlite3 *db = NULL;
struct stmt_list *stmt_list = NULL;

/* DATABASE */

void sqlite_assert_eq(int rv, int rv_expected) {
    db_assert_eq(db, rv, rv_expected);
}
void sqlite_assert(int rv) {
    sqlite_assert_eq(rv, SQLITE_OK);
}

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

sqlite3_stmt *stmt(sqlite3_stmt **ps, const char *q) {
    return db_stmt(db, &stmt_list, ps, q);
}
void stmts_finalize(void) {
    db_stmts_finalize(stmt_list);
    stmt_list = NULL;
}

void db_attach(const char *db_file, const char *table) {
    char *sql = NULL;
    int rv;
    rv = asprintf(&sql, "ATTACH DATABASE ? as %s", table);
    (void)rv;
    sqlite3_stmt *s = stmt(NULL /* Not stored, finalize needed */, sql);
    ASSERT_SQLITE(sqlite3_bind_text(s, 1, db_file, strlen(db_file), NULL));
    rv = sqlite3_step(s);
    sqlite_assert_eq(rv, SQLITE_DONE);
    sqlite3_finalize(s);  /* Delete because not stored. */
}
void db_attach_tmp(const char *db_file) {
    db_attach(db_file, "tmp");
}


// ephemeral statements, not cached
void db_sql0(const char *sql) {
    char *err = NULL;
    sqlite3_exec(db, sql, NULL, NULL, &err);
    if (err) {
        ERROR("db_sql0: %s\n", err);
    }
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
