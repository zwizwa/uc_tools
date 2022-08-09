#ifndef MOD_SWITCH_PROTOCOL
#define MOD_SWITCH_PROTOCOL

/* Bootloader serial port read/write routines that do nothing except
   for attempting to switch to the application. */

#include "gdbstub_ctrl.h"

struct gdbstub_ctrl bootloader_stub_ctrl;

uint32_t count = 0;
uint32_t bootloader_switch_protocol_read1(uint8_t *buf) {
        buf[0] = '!';
        count--;
        return 1;
}
uint32_t bootloader_switch_protocol_read(uint8_t *buf, uint32_t size) {
    if (count > 0) {
        return bootloader_switch_protocol_read1(buf);
    }
    else {
        return 0;
    }
}
void bootloader_switch_protocol_write(const uint8_t *buf, uint32_t size) {
    if (!flash_null(_config.switch_protocol)) {
        ensure_started(&bootloader_stub_ctrl);
        _config.switch_protocol(buf, size);
        return;
    }
    count += size;
}

/*
struct gdbstub_ctrl bootloader_stub_ctrl;
uint32_t null_read(uint8_t *buf, uint32_t size) {
    return 0;
}
void null_write(const uint8_t *buf, uint32_t size) {
}
BOOTLOADER_SERVICE(null_read, null_write, NULL)
*/

#endif
