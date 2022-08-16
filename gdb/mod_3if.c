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
       application protocol.  Let's not worry about it yet. */
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
