#define MMAP_FILE_LOG LOG

#include "mod_minmax.c"

#include "mmap_file.h"
#include "macros.h"


void test(void) {
    struct mmap_file r;
    if (1) {
        char *buf1 = mmap_file_open(&r, "/tmp/123", 100);
        strcpy(buf1, "Test123");
        LOG("buf1 = %p, %s\n", buf1, buf1);
        mmap_file_sync(&r);
    }
    if (1) {
        char *buf2 = mmap_file_reserve(&r, 10000);
        LOG("buf2 = %p, %s\n", buf2, buf2);
        mmap_file_close(&r);
    }
    if (1) {
        const char *buf3 = mmap_file_open_ro(&r, "/tmp/123");
        LOG("buf3 = %p, %d, %s\n", buf3, r.size, buf3);
    }

    // If it's read-only, this will crash:
    //buf[0] = 123;

}

int main(void) {
    LOG("test: %s\n", __FILE__);
    system("rm -f /tmp/123");
    // Run it twice, once without pre-existing file, once with.
    test();
    test();

    struct minmax_map minmax_map;

    minmax_open(&minmax_map, "/tmp/test.raw", 8);

    return 0;
}
