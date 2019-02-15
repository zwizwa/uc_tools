#if 0
// Stub for info calls.
int info_putchar(int c) { return 0; }
#else
// FIXME: just for bootstrapping some low-level code.
#define KEEP __attribute__((section(".keep")))
#include <stdint.h>
#include <string.h>
#define INFO_LOGSIZE 10
#define INFO_SIZE (1<<INFO_LOGSIZE)
#define INFO_MASK (INFO_SIZE-1)
uint8_t info_buf[INFO_SIZE];
uint32_t info_write_next = 0;
uint32_t info_read_next = 0;
KEEP void info_clear(void) {
    memset(info_buf,0,INFO_SIZE);
    info_write_next = 0;
    info_read_next = 0;
}
KEEP int info_putchar(int c) {
    uint32_t offset = (info_write_next++) & INFO_MASK;
    info_buf[offset] = c;
    return 0;
}
KEEP uint32_t info_read(uint8_t *buf, uint32_t len) {
    uint32_t nb = 0;
    for(;;) {
        if (!len) return nb;
        int32_t todo = info_write_next - info_read_next;
        if (!todo) return nb;
        if (todo < INFO_SIZE) {
            *buf++ = info_buf[info_read_next & INFO_MASK];
        }
        else {
            *buf++ ='?'; // be verbose about buffer overrun
        }
        len--;
        info_read_next++;
        nb++;
    }
}
#endif
