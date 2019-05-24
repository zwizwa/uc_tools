#ifndef HW_BOOTLOADER_H
#define HW_BOOTLOADER_H
#include "bootloader.h"
#if defined(HW_MACHINE) && STM32F407 == HW_MACHINE
#include "hw_stm32f407.h"
#else
// Default is the first architecture.
#include "hw_stm32f103.h"
#endif
static inline void bootloader_blink_tick(uint32_t gpio, uint32_t pin,
                                         uint32_t *counter) {
    hw_gpio_write(gpio,pin,((*counter)++>>17)&1);
    bootloader_poll();
    (*counter)++;
}
static inline void bootloader_blink_loop(uint32_t gpio, uint32_t pin) {
    uint32_t counter = 0;
    for(;;) {
        bootloader_blink_tick(gpio, pin, &counter);
    }
}
static inline void application(void *vector_table) {
    hw_app_boot(vector_table);
}


void hw_board_osc_8Mhz(void);

#endif

