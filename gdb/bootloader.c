/* STM32F103 bootloader based on custom gdbstub code and libopencm3

   This code implements a subset of the GDB RSP Remote Serial Protocol
   for a bare metal application that has no concept of tasks.

   The "current task" as presented to GDB is fake.  The stub runs in
   polling mode from the main applications to serve GDB memory get/set
   requests.  There is however a current stack frame, part of the main
   application's main stack, which can be used to execute machine
   code.  The "continue" mechanism is abused to perform GDB's "print"
   or "call" operation on machine code.

   Files:

     cdcacm_desc.c           USB device descriptors for CDC ACM virtual serial port.
     gdbstub_api.h           Datastructures shared between bootloader and application
     gdbstub.[ch]            GDB Remote Serial Protocol server library
     rsp_packet.[ch]         RSP packet handling code
     memory.c                Device memory access for GDB server.
     bootloader.c            Boot loader main application

*/

#include "generic.h"

//#define C_DELAY hw_delay[2]


#include <stdlib.h>
#include <string.h>

#include "gdbstub_api.h"
#include "gdbstub.h"
#include "bootloader.h"


#define POLL_TABLE_SIZE 16
static gdbstub_fn_poll poll_table[POLL_TABLE_SIZE];
static uint32_t poll_used = 0;
static void poll_add (gdbstub_fn_poll fn) {
    if (poll_used < POLL_TABLE_SIZE) {
        poll_table[poll_used++] = fn;
    }
}
static void poll_reset (void) {
    poll_used = 0;
}

/* Table of services provided by bootloader to application. */

/* Instantiate gdbstub state and bind it to bootloader io */
GDBSTUB_INSTANCE(bootloader_stub, gdbstub_default_commands);
uint32_t bootloader_read(uint8_t *buf, uint32_t size) {
    return gdbstub_read(&bootloader_stub, buf, size);
}
void bootloader_write(const uint8_t *buf, uint32_t size) {
    gdbstub_write(&bootloader_stub, buf, size);
}

/* Stored in Flash, fixed location.  See stm32f1.ld */
const struct gdbstub_service service SERVICE_SECTION = {
    .add   = poll_add,
    .reset = poll_reset,
    .rsp_io = {
        .read   = bootloader_read,
        .write  = bootloader_write,
    },
    .io = &io,
    .stub = &bootloader_stub,
};

/* Connect serial port to the GDB RSP state machine. */
const struct gdbstub_io *io = &service.rsp_io;



extern const char gdbstub_memory_map[];


uint32_t ticks;
int32_t timeout_done;

void poll_app(void) {
    for (uint32_t i = 0; i < poll_used; i++) {
        if (poll_table[i]) poll_table[i]();
    }
}

// Hardware routines, defined elsewhere
void hw_bootloader_usb_init(void);
void hw_bootloader_usb_poll(void);

void bootloader_init(void) {
    hw_bootloader_usb_init();
    poll_reset();
}
void bootloader_poll(void) {
    hw_bootloader_usb_poll();
    poll_app();
}
