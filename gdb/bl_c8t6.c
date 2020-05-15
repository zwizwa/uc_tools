#include "hw_bootloader.h"
#include "gdbstub.h"
#include "gdbstub_api.h"

const char gdbstub_memory_map[] = GDBSTUB_MEMORY_MAP_STM32F103C8;
const uint32_t flash_page_size_log = 10; // 1k


/* Config is stored in a separate Flash block and overwritten when we
   flash the application code.  To make the code more robust, the case
   of an empty (all 0xFF) flash block is handled. */
struct gdbstub_config _config_default __attribute__ ((section (".config_header"))) = {
    .bottom = 0x8002800  // allow config overwrite
};


// #include "inline.dbg_pin.c" // KEEPs for interactive toggling and reading

// ARM Cortex-M3 STM32F103C8T6 STM32 core board development board
// A simple, bare bones board with USB.  I have 5, one is made by:
// http://www.lctech-inc.com/Hardware/Detail.aspx?id=0172e854-77b0-43d5-b300-68e570c914fd
// The other 4 seem functionally equivalent but have different layout and silkscreen marks.

// EDIT: Also works on narrower bare-bones boards.

extern struct gdbstub bootloader_stub;

#define LED GPIOC,13
static uint32_t counter = 0;
void bootloader_tick(void) {
    bootloader_blink_tick(LED, &counter);
}





int main(void) {
    rcc_clock_setup_in_hse_8mhz_out_72mhz();
    rcc_periph_clock_enable(RCC_GPIOB);
    rcc_periph_clock_enable(RCC_GPIOC);
    hw_gpio_config(LED,HW_GPIO_CONFIG_OUTPUT);
    bootloader_init();

    /* When BOOT0==0 (boot from flash), BOOT1 is ignored, so we can
     * use it as an application start toggle. */
    uint32_t boot1 = hw_gpio_read(GPIOB,2);
    if (boot1) { _config.start(); }

    if (!flash_null(_config.start)) _config.start();


    /* For now, the loop takeover routine is implemented on a per
       loader basis as it is still under test.

       An application can take over the main loop after its start()
       routine has executed from the poll loop in a previous cycle.

       The loop() function can e.g. be used to switch the uC to
       ISR-only mode, by executing the bootloader tick method from the
       USB ISR.

       Note that the bootloader does not support interrupts.  The
       application contains the interrupt vector table. */

    for (;;) {
        /* Application can request to take over the main loop.  This
         * is useful for running USB in ISR.  This mechanism is
         * separate from start() because start() is executed from the
         * main loop already, and needs to return. */
        if ((bootloader_stub.flags & GDBSTUB_FLAG_STARTED) && (_config.loop)) {
            bootloader_stub.flags |= GDBSTUB_FLAG_LOOP;
            _config.loop(&bootloader_tick);
            // NOT REACHED
        }
        /* By default, just poll USB in the main loop. */
        else {
            bootloader_tick();
        }
    }
    return 0;
}


