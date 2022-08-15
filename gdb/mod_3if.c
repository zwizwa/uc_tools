#ifndef MOD_3IF
#define MOD_3IF


#include "mod_monitor_3if.c"
#include "gdbstub_ctrl.h"
struct gdbstub_ctrl bootloader_stub_ctrl;

struct monitor_3if monitor_3if;

uint32_t bootloader_3if_read(uint8_t *buf, uint32_t size) {
    return cbuf_read(&monitor_3if.out, buf, size);
}
void bootloader_3if_write(const uint8_t *buf, uint32_t size) {
    /* This needs to support protocol switching, which is based on
       bootloader protocol being sufficiently different from any
       application protocol.  Let's not worry about it yet. */
    monitor_3if_write(&monitor_3if, buf, size);
}


#endif
