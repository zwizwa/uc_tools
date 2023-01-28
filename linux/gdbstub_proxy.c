/* GDB proxy server.

   This code implements GDB RSP in terms of abstract memory access.
   Can be used e.g. to serve core dumps, or remote debugging over some
   opaque protocol. */

// FIXME: This is hardcoded to host 20k STM32F103 dumps at 0x20000000

#define LOG(...) fprintf(stderr, __VA_ARGS__)

#include "gdb/gdbstub.h"
const char gdbstub_memory_map[] = GDBSTUB_MEMORY_MAP_STM32F103CB;
struct gdbstub_config _config;

// FIXME: These are not in the lib.  Why?
#include "gdb/rsp_packet.c"
#include "gdb/gdbstub.c"

#include "assert_write.h"
#include "tcp_tools.h"

GDBSTUB_INSTANCE(gdbstub, gdbstub_default_commands);

uint8_t mem[0x5000] = {};


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
    if (addr <   0x20000000) return 0x55;
    if (addr >=  0x20005000) return 0x55;
    return mem[addr - 0x20000000];
}

void serve(int fd_in, int fd_out) {
    uint8_t buf[1024];
    for(;;) {
        ssize_t n_stdin = read(fd_in, buf, sizeof(buf)-1);
        if (n_stdin == 0) break;
        ASSERT(n_stdin > 0);
        buf[n_stdin] = 0;
        // LOG("I:%d:%s\n",n_stdin,buf);

        gdbstub_write(&gdbstub, buf, n_stdin);
        uint32_t n_stub = gdbstub_read_ready(&gdbstub);
        if (n_stub > sizeof(buf)-1) { n_stub = sizeof(buf)-1; }
        gdbstub_read(&gdbstub, buf, n_stub);
        buf[n_stub] = 0;
        // LOG("O:%d:%s\n", n_stub, buf);

        assert_write(fd_out, buf, n_stub);
        // FLUSH?
    }
}

int main(int argc, char **argv) {
    ASSERT(argc >= 2);

    /* Load the SRAM image */
    FILE *f;
    ASSERT(NULL != (f = fopen(argv[1], "r")));
    uint32_t rv = fread(mem, 1, sizeof(mem), f);
    ASSERT(sizeof(mem) == rv);
    fclose(f);

    /* uc abort routine dumps 12 registers at the end of RAM
       containing r4 to pc */
    uint32_t reg_dump = 4 * 12;
    memcpy(gdbstub.reg + 4,
           mem + sizeof(mem) - reg_dump,
           reg_dump);

    /* The GDB server doesn't produce any events by itself, so we can
       block on file descriptor, pass to GDB server, read its response
       and repeat. */
    if (0) {
        // ASSERT(argc > 0);
        // LOG("%s on stdio\n", argv[0]);
        serve(0, 1);
    }
    else {
        uint16_t listen_port = 20000;
        int fd_listen = assert_tcp_listen(listen_port);
        int rv = 0;
        while (rv == 0) {
            int fd_con = assert_accept(fd_listen);
            LOG("accepted %d\n", listen_port);
            serve(fd_con, fd_con);
        }
    }
    return 0;
}
