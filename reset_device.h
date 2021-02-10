#ifndef RESET_DEVICE_H
#define RESET_DEVICE_H

#include "base.h"
#include "libopencm3/stm32/f1/iwdg.h"

static inline void reset_device_ms(int ms) {
    // Disengage the USB speed pullup connected to A12 (D+) to signal
    // host that we're no longer there.
    hw_gpio_config(GPIOB,5,HW_GPIO_CONFIG_INPUT);

    // Note that this does not work to reset the device:
    // SCB_AIRCR |= SCB_AIRCR_SYSRESETREQ;
    // I think it only resets the core, not the peripherals.

    // The simplest way I found is to use the watchdog timer.
    iwdg_set_period_ms(ms);
    iwdg_start();

    // Sit here until it restarts.
    for(;;) { volatile int i = 0; (void)i; }
}
static inline void reset_device(void) {
    reset_device_ms(1000);
}



#endif

