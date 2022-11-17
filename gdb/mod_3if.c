#ifndef MOD_3IF
#define MOD_3IF

#include "mod_monitor_3if.c"
#include "gdbstub_ctrl.h"
struct gdbstub_ctrl bootloader_stub_ctrl;

struct monitor_3if monitor_3if;

uint32_t bootloader_3if_read(uint8_t *buf, uint32_t size) {
    return cbuf_read(monitor_3if.out, buf, size);
}
void bootloader_3if_write(const uint8_t *buf, uint32_t size) {
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
    if (0 != monitor_3if_write(&monitor_3if, buf, size)) {
        ensure_started(&bootloader_stub_ctrl);
        _config.switch_protocol(buf, size);
        return;
    }
}
struct cbuf monitor_3if_out; uint8_t monitor_3if_out_buf[256];
struct cbuf monitor_3if_out; uint8_t monitor_3if_out_buf[256];
uint8_t ds_buf[32];
uint8_t rs_buf[32];
void bootloader_3if_init(void) {
    CBUF_INIT(monitor_3if_out);
    monitor_3if_init(&monitor_3if, &monitor_3if_out, ds_buf, rs_buf);
}

#endif
