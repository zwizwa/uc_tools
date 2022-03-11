/* GDB proxy server.

   This code implements GDB RSP in terms of abstract memory access.
   Can be used e.g. to serve core dumps, or remote debugging over some
   opaque protocol. */

#include "gdb/gdbstub.h"
const char gdbstub_memory_map[] = GDBSTUB_MEMORY_MAP_STM32F103CB;
struct gdbstub_config _config;

// FIXME: These are not in the lib.  Why?
#include "gdb/rsp_packet.c"
#include "gdb/gdbstub.c"

#include "assert_write.h"

struct gdbstub s;

// All write access is stubbed out.
int32_t flash_erase(uint32_t addr, uint32_t size) {
    return 0;
}
int32_t flash_write(uint32_t addr, const uint8_t *b_buf, uint32_t len) {
    return 0;
}
int32_t mem_write(uint32_t addr, uint8_t val) {
    return E_OK;
}
int32_t mem_write32(uint32_t addr, uint32_t val) {
    return E_OK;
}

uint8_t mem_read(uint32_t addr) {
    return 0;
}


int main(int argc, char **argv) {
    /* The GDB server doesn't produce any events by itself, so we can
       block on stdin here, pass to GDB server, read its response and
       repeat. */
    uint8_t buf[64];
    for(;;) {
        ssize_t n_stdin = read(0, buf, sizeof(buf));
        ASSERT(n_stdin > 0);
        gdbstub_write(&s, buf, n_stdin);
        uint32_t n_stub = gdbstub_read_ready(&s);
        if (n_stub > sizeof(buf)) { n_stub = sizeof(buf); }
        gdbstub_read(&s, buf, n_stub);
        assert_write(1, buf, n_stub);
    }
    return 0;
}
