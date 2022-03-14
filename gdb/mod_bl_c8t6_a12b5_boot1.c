/* Configuratiom */
#ifndef GDBSTUB_BOOT1_START
#error NEED GDBSTUB_BOOT1_START
#endif

#include "gdbstub.h"
#include "gdbstub_api.h"
#include "hw_bootloader.h"

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

void ensure_started(struct gdbstub_ctrl *stub_ctrl);


#ifndef GDBSTUB_RSP_ENABLED
#define GDBSTUB_RSP_ENABLED 1
#endif



#if GDBSTUB_RSP_ENABLED
BOOTLOADER_DEFAULT_SERVICE()
#else
uint32_t null_read(uint8_t *buf, uint32_t size) {
    return 0;
}
void null_write(const uint8_t *buf, uint32_t size) {
}
BOOTLOADER_SERVICE(null_read, null_write, NULL)
#endif




#define LED GPIOC,13
static uint32_t counter = 0;
void bootloader_tick(void) {
    bootloader_blink_tick(LED, &counter);
}





int main(void) {
    rcc_clock_setup_in_hse_8mhz_out_72mhz();
    rcc_periph_clock_enable(RCC_GPIOC);
    hw_gpio_config(LED,HW_GPIO_CONFIG_OUTPUT);

    /* For modded board: 1k5 between A12 and B5 with the original R10
       pullup removed.  We set B5 high here to assert the pullup and
       signal the host we are a full speed device.  This does two
       things: on reset the pin will be de-asserted, signalling the
       host we are disconnected.  Additionally it places the pullup
       under program control. */
    rcc_periph_clock_enable(RCC_GPIOB | RCC_AFIO);
    hw_gpio_high(GPIOB,5);
    hw_gpio_config(GPIOB,5,HW_GPIO_CONFIG_OUTPUT);

    bootloader_init();

    /* When BOOT0==0 (boot from flash), BOOT1 is ignored by the STM
       boot ROM, so we can use it as an application start toggle. */
    /* FIXME: Find out why this is not 100% reliable. */
    uint32_t boot1 = hw_gpio_read(GPIOB,2);
    if (GDBSTUB_BOOT1_START(boot1) && !flash_null(_config.start)) {
        ensure_started(&bootloader_stub_ctrl);
    }


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
        if ((bootloader_stub_ctrl.flags & GDBSTUB_FLAG_STARTED) && (_config.loop)) {
            bootloader_stub_ctrl.flags |= GDBSTUB_FLAG_LOOP;
            _config.loop(&bootloader_tick);
            /* If .loop() is implemented correctly this should not
               return.  However, in case it does, we fall through to
               bootloader_tick() below. */
        }
        /* By default, poll USB in the main loop. */
        bootloader_tick();
    }
    return 0;
}
