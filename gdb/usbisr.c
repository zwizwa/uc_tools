// Test for running USB from interrupt.
// Note that this needs a recent bootloader to work.

#include "base.h"
#include "gdbstub_api.h"
#include <string.h>


uint32_t nb_cmds;
static void usbisr_write(const uint8_t *buf, uint32_t len) {
    nb_cmds += len;
}
static uint32_t usbisr_read(uint8_t *buf, uint32_t len) {
    if (nb_cmds) {
        nb_cmds--;
        // {packet,4}
        buf[0] = 0;
        buf[1] = 0;
        buf[2] = 0;
        buf[3] = 1;
        buf[4] = _service.stub->flags;
        return 5;
    }
    else return 0;
}
const struct gdbstub_io usbisr_io = {
    .read  = usbisr_read,
    .write = usbisr_write,
};




/* STARTUP */
void switch_protocol(const uint8_t *buf, uint32_t size) {
    *_service.io = (struct gdbstub_io *)(&usbisr_io);
    (*_service.io)->write(buf, size);
}
void start(void) {
    hw_app_init();
}

/* MAIN LOOP

   config.loop is defined, so the bootloader will call loop() once
   start() has been executed.

   Typically start() and switch_protocol() will be executed when a
   non-GDBRSP message is sent to the serial port.

   loop() is passed the main bootloader poll routine.  We will call
   this from ISR, leaving a main loop that only sleeps.
*/
static gdbstub_fn_poll bl_poll;
void loop(gdbstub_fn_poll _bl_poll) {
    bl_poll = _bl_poll;
    nvic_enable_irq(NVIC_USB_LP_CAN_RX0_IRQ);
    for(;;) { __asm volatile ("wfi"); }
}
void usb_lp_can_rx0_isr(void) {
    bl_poll();
}

int main(void) { start(); }

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "USB ISR test";
const char config_serial[]       CONFIG_DATA_SECTION = "000000000000000000000000";
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;
const char config_version[]      CONFIG_DATA_SECTION = BUILD;

struct gdbstub_config config CONFIG_HEADER_SECTION = {
    .manufacturer    = config_manufacturer,
    .product         = config_product,
    .serial          = config_serial,
    .firmware        = config_firmware,
    .version         = config_version,
    .start           = start,
    .switch_protocol = switch_protocol,
    .loop            = loop
};




