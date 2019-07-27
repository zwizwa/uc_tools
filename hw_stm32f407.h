#ifndef HW_STM32F407_H
#define HW_STM32F407_H

/* This code is based for the larger part on libopencm3 code.
   E.g. hw_usart_enable() is a copy of libopencm3's usart_enable(), etc.. */

/* Parameterization of generic STM32F code */
#define HW_LOOPS_PER_US 12
#define HW_USB_DRIVER &otgfs_usb_driver

//#warning F407

#include <libopencm3/stm32/gpio.h>


#include "hw_stm32f_common.h"
// FIXME: 407 versions of this
#include <libopencm3/stm32/f4/rcc.h>
#include <libopencm3/stm32/f4/timer.h>
#include <libopencm3/stm32/f4/gpio.h>
#include <libopencm3/stm32/f4/usart.h>
#include <libopencm3/stm32/f4/spi.h>


/* Gpio */
INLINE void hw_gpio_high(uint32_t gpio, uint32_t pin) {
    GPIO_BSRR(gpio) = 1 << pin;
}
INLINE void hw_gpio_low(uint32_t gpio, uint32_t pin) {
    /* Note that the F4-7 does not have a BRR register like the F103.
     * The high bits of the BSRR are used for reset.  This here needs
     * to be validated. */
    //GPIO_BRR(gpio) = 1 << pin;
    GPIO_BSRR(gpio) = 1 << (pin + 16);
}



#endif // HW_STM32F407_H


