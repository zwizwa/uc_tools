#include "mmap_array.h"
#include "macros.h"
int main(void) {
    LOG("test: %s\n", __FILE__);
    struct mmap_array r;
    mmap_array_init(&r, "/tmp/%d", 123);
    volatile uint8_t *buf = mmap_array_buf(&r, 100);
    LOG("buf = %p\n", buf);
    //(void)(*buf);
    mmap_array_sync(&r);
    return 0;
}
