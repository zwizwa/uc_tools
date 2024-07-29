#ifndef BOARD_H
#define BOARD_H
#include "generic.h"
#include "gdbstub_api.h"


//usbd_device *bootloader_init(void);
//void bootloader_poll(usbd_device *usbd_dev);
void bootloader_init(void);
void bootloader_poll(void);

uint32_t bootloader_read(uint8_t *buf, uint32_t size);
void     bootloader_write(const uint8_t *buf, uint32_t size);

static inline void bootloader_loop(void) {
    for(;;) {
        bootloader_poll();
    }
}
void bootloader_poll_add (gdbstub_fn_poll fn);
void bootloader_poll_reset (void);

void bootloader_io_reset(void);

extern uint8_t _ebss;
extern uint8_t _stack;
extern struct gdbstub bootloader_stub;
extern struct gdbstub_ctrl bootloader_stub_ctrl;

// Instantiate the service struct.  This pulls in all dependencies.
#define BOOTLOADER_SERVICE(bl_read, bl_write, bl_stub) \
const struct gdbstub_service service SERVICE_SECTION = { \
    .add   = bootloader_poll_add, \
    .reset = bootloader_poll_reset, \
    .rsp_io = { .read = bl_read, .write = bl_write }, \
    .io = &io, \
    .stub = bl_stub, \
    .stack_lo = &_ebss, \
    .stack_hi = &_stack, \
};

#define BOOTLOADER_DEFAULT_SERVICE() \
    GDBSTUB_INSTANCE(bootloader_stub, gdbstub_default_commands); \
    BOOTLOADER_SERVICE(bootloader_read, bootloader_write, &bootloader_stub)



#endif // BOARD_H
