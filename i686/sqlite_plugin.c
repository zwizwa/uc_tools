/* sqlite plugin, see plugin.h
   - sqlite on kernel_blockdev through mod_sqlite_vfs.c
   - words: sqtest, sqcrash, sqformat
   - make sqlite_plugin.bin; tether_udp plugin <host> sqlite_plugin.bin */

#include "plugin.h"
#define LOG(...) kernel_infof(__VA_ARGS__)
#include "tools.h"
#include "boot_config.h"
#include "hw_i686_io.h"
#include <string.h>

#include "mod_sqlite_vfs.c"

static uint8_t sqlite_heap[8 << 20] __attribute__((aligned(8)));
static int sqtest_row(void *ctx, int n, char **val, char **col) {
    for (int i = 0; i < n; i++) LOG("%s=%s ", col[i], val[i] ? val[i] : "NULL");
    LOG("\n");
    return 0;
}
static sqlite3 *db;
static sqlite3 *sqopen(void) {
    static int init;
    if (!kernel_blockdev) return 0;
    if (!init) {
        __asm__ volatile ("fninit");
        sqlite3_config(SQLITE_CONFIG_HEAP, sqlite_heap, sizeof(sqlite_heap), 64);
        if (sqlite3_initialize()) {
            LOG("sqlite init failed\n");
            return 0;
        }
        init = 1;
    }
    if (!db) {
        if (sqv_init(kernel_blockdev)) return 0;
        if (sqlite3_open("sda3", &db)) {
            LOG("sqlite open failed\n");
            sqlite3_close(db);
            db = 0;
        }
    }
    return db;
}
/* Closes the database first. */
static void sqformat(void) {
    if (!kernel_blockdev) return;
    if (db) { sqlite3_close(db); db = 0; }
    sqv_format(kernel_blockdev);
}
static void sqexec(const char *sql) {
    sqlite3 *db = sqopen();
    char *err = 0;
    if (!db) return;
    int rv = sqlite3_exec(db, sql, sqtest_row, 0, &err);
    if (rv) LOG("sqlite rv=%d %s\n", rv, err ? err : "");
    sqlite3_free(err);
}
static void sqtest(void) {
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
   Restarts the kernel from the last 3if push, so only use after one.
   The plugin needs to be loaded again after that. */
static void sqcrash(void) {
    if (!sqopen()) return;
    sqv.crash_after = 20;
    sqv.crash = reexec_3if;
    sqexec("insert into boot(t) select hex(randomblob(500)) from"
           " (with recursive c(x) as (select 1 union all select x+1 from c"
           "  where x < 2000) select x from c);");
}

/* First word of the line */
static int word_is(const uint8_t *line, uint32_t len, const char *word) {
    uint32_t n = mini_strlen(word);
    return len > n && !memcmp(line, word, n) && line[n] <= ' ';
}
static int sqlite_cmd(const uint8_t *line, uint32_t len) {
    if      (word_is(line, len, "sqtest"))  sqtest();
    else if (word_is(line, len, "sqcrash")) sqcrash();
    else if (word_is(line, len, "sqformat")) sqformat();
    else return -1;
    return 0;
}
static struct plugin sqlite_plugin = {
    .name = "sqlite",
    .cmd  = sqlite_cmd,
};

/* Called by 3if JSR, in the isr.  On a version mismatch the kernel
   symbols are wrong, so return without calling into the kernel. */
extern uint8_t __plugin_bss_start, __plugin_bss_end;
__attribute__((section(".plugin_start")))
void plugin_start(void) {
    if (mini_strcmp(kernel_version, KERNEL_VERSION)) return;
    boot_config.plugin = 0;
    memset(&__plugin_bss_start, 0, &__plugin_bss_end - &__plugin_bss_start);
    boot_config.plugin = &sqlite_plugin;
    LOG("plugin %s\n", sqlite_plugin.name);
}
