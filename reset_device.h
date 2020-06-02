#ifndef RESET_DEVICE_H
#define RESET_DEVICE_H

#include "base.h"
#include "libopencm3/stm32/f1/iwdg.h"

static inline void reset_device(void) {
    // Disengage the USB pull speed pullup on A12 (D+) to signal
    // host that we're no longer there.
    hw_gpio_config(GPIOB,5,HW_GPIO_CONFIG_INPUT);

    // Note that this does not work to reset the device:
    // SCB_AIRCR |= SCB_AIRCR_SYSRESETREQ;
    // I think it only resets the core, not the peripherals.

    // The simplest way I found is to use the watchdog timer.
    // Start it with its default 512 milliseconds timeout.  That
    // also gives some unplug/replug delay.
    iwdg_start();

    // Sit here for a bit to allow it to restart.
    hw_busywait_ms(3000);
    // If that doesn't happen for some reason we will at least
    // continue.
}

#endif

