#ifndef HW_HYMINISTM32V_H
#define HW_HYMINISTM32V_H

// Common bootloader config for www.powermcu.com HY-MiniSTM32V with STM32F103VCT6
// see hi-ministm32v_sch.pdf

#include "hw_bootloader.h"
#include "gdbstub.h"
#include "gdbstub_api.h"

// xC: high density device: 2k Flash pages
const uint32_t flash_page_size_log = 11;
const char gdbstub_memory_map[] = GDBSTUB_MEMORY_MAP_STM32F103VC;

/* Config is stored in a separate Flash block and overwritten when we
   flash the application code.  To make the code more robust, the case
   of an empty (all 0xFF) flash block is handled. */
struct gdbstub_config _config_default __attribute__ ((section (".config_header"))) = {
    .bottom = GDBSTUB_ADDR_CONF  // allow config overwrite
};

#define LED     GPIOB,0
#define USB_NEN GPIOB,7  // enable, active low

static inline void hyministm32v_init(void) {
    rcc_clock_setup_in_hse_8mhz_out_72mhz();
    rcc_periph_clock_enable(RCC_GPIOB);

    // USB ENABLE
    hw_gpio_low(USB_NEN);
    hw_gpio_config(USB_NEN, HW_GPIO_CONFIG_OUTPUT);

    // LED
    hw_gpio_config(LED, HW_GPIO_CONFIG_OUTPUT);
}

#endif

