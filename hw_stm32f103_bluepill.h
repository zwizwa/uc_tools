#ifndef HW_STM32F103_BLUEPILL_H
#define HW_STM32F103_BLUEPILL_H

/* Routines specific to the STM "Blue Pill" (BP) boards. */
#include "hw_stm32f103.h"


/* All BP boards I've seen have a hardwired USB pullup resistor
   connected to A12 (USB D+).  Please do not do this when designing
   new boards.  I've been modding these by removing that hardwired
   pullup and connecting it to B5, which is the closest ordinary pin
   on the BP board.  This allows direct control over the usb
   connect/disconnect detection without power cycling or replugging
   the board.  See bl_c8t6_a12b5.c

   The routine below fakes a replug event by shorting PA12 to ground
   for a short amount of time.  See usb.c in
   https://github.com/rogerclarkmelbourne/STM32duino-bootloader

   Note that instead of doing this at startup, we do this at
   shutdown/reboot.  This makes it so that the uc_tools bootloader
   does not need to contain this hack.  Instead it can go into the
   tethering toolbox.
*/

#define USB_DP GPIOA,12

void hw_usb_dp_reset(void) {
    hw_gpio_low(USB_DP);
    hw_gpio_config(USB_DP, HW_GPIO_CONFIG_OUTPUT);
    hw_busyloop_us(2000); // What is a good value here?
}


#endif



