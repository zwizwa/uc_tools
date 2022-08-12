#ifndef MOD_3IF
#define MOD_3IF


/* Experimental Forth monitor application to replace GDBSTUB. */

#define RAMLEN 0x100
#include "mod_forth_dsl.c"
//#include "mod_forth_dsl_cprim.c"
#include "gdbstub_ctrl.h"
struct gdbstub_ctrl bootloader_stub_ctrl;

struct forth_dsl_state forth_dsl_state;

uint32_t bootloader_3if_read(uint8_t *buf, uint32_t size) {
    return cbuf_read(&forth_dsl_state, buf, size);
}
void bootloader_3if_write(const uint8_t *buf, uint32_t size) {
    /* This needs to support protocol switching, which is based on
       bootloader protocol being sufficiently different from any
       application protocol.  Let's not worry about it yet. */
    forth_dsl_write(&forth_dsl_state, buf, size);
}

#endif
