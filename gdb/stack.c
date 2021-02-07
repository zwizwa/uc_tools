#include "stack.h"
#include "gdbstub_api.h"

void mark_stack(void) {
    /* FIXME: This is very much undefined behavior and will need to be
       written in assembler to be able to guarantee anything.  We just
       add a guard offset in case there is some stack allocation going
       on for the loop.  The stack usage of the application will be
       more than that. */
    uint8_t *bottom = 0;
    __asm__ volatile ("mov %0, sp\n\t" : "=r" ( bottom ) );
    bottom -= 100;
    for(uint8_t *p=_service.stack_lo; p<bottom; p++) *p = 0x55;
    //uintptr_t nb = bottom - (uint8_t*)_service.stack_lo;
    //infof("stack %x %d\n", _service.stack_lo, nb);
}
uintptr_t check_stack(void) {
    uint8_t *bottom = 0;
    __asm__ volatile ("mov %0, sp\n\t" : "=r" ( bottom ) );
    uintptr_t unused = 0;
    for(uint8_t *p=_service.stack_lo; p<bottom; p++) {
        if (*p != 0x55) {
            unused = p - (uint8_t*)_service.stack_lo;
            break;
        }
    }
    return unused;
}

/* This is a hack, only here for debugging, such that GDB can allocate
   memory when executing expressions.  It does not work as normal
   malloc() */
uint8_t *bump_start = 0;
void bump_reset(void) {
    bump_start = _service.stack_lo;
    //infof("free %x\n", bump_start);
}
void *bump_alloc(uintptr_t bytes) {
    if (!bump_start) bump_reset();
    void *mem = bump_start;
    const uint32_t mask = sizeof(uintptr_t)-1;
    if (bytes & mask) {
        bytes = (bytes + mask) & mask; // Align
    }
    bump_start += bytes;
    //infof("malloc %x %d\n", mem, bytes);
    return mem;
}
