#ifndef MOD_MONITOR
#define MOD_MONITOR

/* Area occupied by bootloader.  Do not write here. */
#define MEM_WRITE_FLASH_START 0x08000000
#define MEM_WRITE_FLASH_ENDX  0x08002800

/* Write to Flash is implemented using the same functions that are
   used to implement in-app Firmware upgrade (fwstream.h) */
#define HW_CPU_MHZ 72
#define MEM_WRITE_LOG(...) // no logging in bootloader
#include "cycle_counter.h"
#include "mod_mem_write_stm32f103.c"
#define TO_FLASH_3IF to_flash_stm
struct monitor_3if;
void to_flash_stm(struct monitor_3if *s);
#include "mod_monitor_3if.c"
#include "gdbstub_ctrl.h"
struct gdbstub_ctrl bootloader_stub_ctrl;

/* STM minimum is 2 bytes. */
#define FLASH_BUFSIZE_LOG 1
#define FLASH_BUFSIZE (1 << FLASH_BUFSIZE_LOG)

struct monitor {
    struct monitor_3if monitor_3if;
    uint8_t out_buf[256 + 16];
    struct cbuf out;
    uint8_t ds_buf[32];
    uint8_t flash_buf[FLASH_BUFSIZE];
    uint8_t last_read_was_full:1;
};
struct monitor monitor;

void to_flash_stm(struct monitor_3if *s) {
    struct monitor *m = (void*)s;
    uint32_t addr = (uint32_t)s->flash;
    uint32_t i = addr % FLASH_BUFSIZE;
    m->flash_buf[i] = s->byte;
    if (i == FLASH_BUFSIZE - 1) {
        hw_mem_write_log(addr-i, m->flash_buf, FLASH_BUFSIZE);
    }
    s->flash++;
}

uint32_t monitor_read(uint8_t *buf, uint32_t size) {
    if (monitor.monitor_3if.poll) {
        // Optional poll routine.
        monitor.monitor_3if.poll(&monitor.monitor_3if);
    }
    uint32_t n = cbuf_read(monitor.monitor_3if.out, buf, size);
    if ((n == 0) && monitor.last_read_was_full) {
        // Pad with zero-size framing to avoid the bad n=0 case.
        buf[0] = 0;
        n=1;
    }
    monitor.last_read_was_full = (n == 64);
    return n;
}
void monitor_write(const uint8_t *buf, uint32_t size) {
    /* This needs to support protocol switching, which is based on
       bootloader protocol being sufficiently different from any
       application protocol.  The following protocols need to be
       supported:

       - SLIP
       - {packet,4}
       - Raw text console, initiated by '\n' or '\r'

       The monitor_3if_write / monitor_3if_push_byte function returns
       0 when the machine is waiting for more instructions, and
       anything else indicates an error.

       At startup it is waiting for a size prefix followed by an
       instruction 0x80-0x8F.  The 3 protocols above will be able to
       send an initial application packet to bypass the 3if monitor.
       E.g. empty SLIP, empty {packet,4} or any string of 2 ASCII
       characters or control characters.
    */
    if (0 != monitor_3if_write(&monitor.monitor_3if, buf, size)) {
        ensure_started(&bootloader_stub_ctrl);
        _config.switch_protocol(buf, size);
        return;
    }
}
void monitor_init(void) {
    CBUF_INIT(monitor.out);
    monitor_3if_init(
        &monitor.monitor_3if,
        &monitor.out,
        monitor.ds_buf);
}

#endif
