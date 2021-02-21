#define MMAP_FILE_LOG LOG

#include "mmap_file.h"
#include "macros.h"

intptr_t get_mipmap(uint32_t level, uint8_t *buf, intptr_t nb) {
    return 0;
}

void test(void) {
    struct mmap_file r;
    uint8_t *buf = mmap_file_open(&r, "/tmp/123", 100);
    LOG("buf = %p\n", buf);
    mmap_file_sync(&r);
    buf = mmap_file_reserve(&r, 10000);
    mmap_file_close(&r);
}

int main(void) {
    LOG("test: %s\n", __FILE__);
    system("rm /tmp/123");
    // Run it twice, once without pre-existing file, once with.
    test();
    test();
    return 0;
}
