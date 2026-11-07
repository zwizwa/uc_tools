/* sqlite test words, moved out of kernel.c.  Not built: becomes the
   first plugin in step 3, see mod_sqlite_vfs.c */

/* sqlite on the type 7f partition, see mod_sqlite_vfs.c */
#include "mod_sqlite_vfs.c"
static uint8_t sqlite_heap[8 << 20] __attribute__((aligned(8)));
static int sqtest_row(void *ctx, int n, char **val, char **col) {
    for (int i = 0; i < n; i++) LOG("%s=%s ", col[i], val[i] ? val[i] : "NULL");
    LOG("\n");
    return 0;
}
static sqlite3 *sqopen(void) {
    static sqlite3 *db;
    if (!db) {
        if (!g_app.part.bd.read) return 0;
        __asm__ volatile ("fninit");
        sqlite3_config(SQLITE_CONFIG_HEAP, sqlite_heap, sizeof(sqlite_heap), 64);
        if (sqlite3_initialize() || sqv_init(&g_app.part.bd) ||
            sqlite3_open("sda3", &db)) {
            LOG("sqlite init failed\n");
            db = 0;
        }
    }
    return db;
}
static void sqexec(const char *sql) {
    sqlite3 *db = sqopen();
    char *err = 0;
    if (!db) return;
    int rv = sqlite3_exec(db, sql, sqtest_row, 0, &err);
    if (rv) LOG("sqlite rv=%d %s\n", rv, err ? err : "");
    sqlite3_free(err);
}
void sqtest(void) {
    sqexec("create table if not exists boot(n integer primary key, t text);"
           "insert into boot(t) values (sqlite_version());"
           "select count(*), max(n) from boot;"
           "pragma integrity_check;");
}
/* Image tether_udp left at 16MB: restart without the BIOS. */
static void reexec_3if(void) {
    cli();
    ((void (*)(void))0x1000000)();
}
/* Crashes halfway through writing the db pages of a big commit.
   Restarts from the last 3if push, so only use after one. */
void sqcrash(void) {
    if (!sqopen()) return;
    sqv.crash_after = 20;
    sqv.crash = reexec_3if;
    sqexec("insert into boot(t) select hex(randomblob(500)) from"
           " (with recursive c(x) as (select 1 union all select x+1 from c"
           "  where x < 2000) select x from c);");
}
