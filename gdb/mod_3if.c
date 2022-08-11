#ifndef MOD_3IF
#define MOD_3IF


/* Experimental Forth monitor application to replace GDBSTUB. */

#include "forth_dsl.h"
#include "gdbstub_ctrl.h"
struct gdbstub_ctrl bootloader_stub_ctrl;

struct forth_dsl_env forth_dsl_env;

uint32_t count = 0;
uint32_t bootloader_3if_read1(uint8_t *buf) {
    buf[0] = '!';
    count--;
    return 1;
}
uint32_t bootloader_3if_read(uint8_t *buf, uint32_t size) {
    if (count > 0) {
        return bootloader_3if_read1(buf);
    }
    else {
        return 0;
    }
}
void bootloader_3if_write(const uint8_t *buf, uint32_t size) {
    /* This needs to support protocol switching, which is based on
       bootloader protocol being sufficiently different from any
       application protocol.  Let's not worry about it yet. */
    forth_dsl_write(&forth_dsl_env, buf, size);
}

#endif
