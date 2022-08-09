#ifndef MOD_3IF
#define MOD_3IF


/* Experimental 3-Instruction Forth monitor application to replace
   GDBSTUB.  Idea is to use a 2-layer architecture: run the gdbstub
   code on host, and use a minimal load/store/execute API on the
   target.  Similar to Staapl, which in turn is inspired by Frank
   Sergeant's https://pages.cs.wisc.edu/~bolo/shipyard/3ins4th.html */

struct gdbstub_ctrl bootloader_stub_ctrl;

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
    count += size;
}

#endif
