#ifndef HW_STM32F_H
#define HW_STM32F_H

/* This code is based for the larger part on libopencm3 code.
   E.g. hw_usart_enable() is a copy of libopencm3's usart_enable(), etc..

   See also:
   hw_stm32f103.h
   hw_stm32f407.h

   NEEDS:
   HW_LOOPS_PER_US
*/



/*
 * This file is part of the libopencm3 project.
 *
 * Copyright (C) 2009 Uwe Hermann <uwe@hermann-uwe.de>
 *
 * This library is free software: you can redistribute it and/or modify
 * it under the terms of the GNU Lesser General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU Lesser General Public License for more details.
 *
 * You should have received a copy of the GNU Lesser General Public License
 * along with this library.  If not, see <http://www.gnu.org/licenses/>.
 */

/* Open Cortex M3 library. */
#include <libopencm3/cm3/common.h>
#include <libopencm3/cm3/nvic.h>
#include <libopencm3/stm32/exti.h>
#include <libopencm3/stm32/dma.h>
#include <libopencm3/stm32/sdio.h>
#include <libopencm3/cm3/scb.h>
#include <libopencm3/stm32/rcc.h>
#include <libopencm3/stm32/gpio.h>
#include <libopencm3/usb/usbd.h>
#include <libopencm3/usb/cdc.h>



/* The easiest way to avoid malloc() on the target is to make sure
   it's not in there.  See infof.c for a printf replacement. */
#define printf  dont_use_printf
#define putchar dont_use_putchar



/* For debugging: keep a function in the firmware image, even if it's
   not referenced.  All otherwise unused functions get
   garbage-collected at link time. */
#define KEEP __attribute__((section(".keep")))


/* Identifier concatenation. */
//#define _CONCAT(X,Y)  X##Y
//#define CONCAT(X,Y)   _CONCAT(X,Y)
//#define GENSYM(sym)   CONCAT(sym,__COUNTER__)



INLINE uint32_t hw_swap(uint32_t val) {
    uint32_t rv;
    __asm__("rev %0, %1 \n"
            : "=r"(rv)
            : "r"(val)
            :);
    return rv;
}


/* Loop based busywait. */
INLINE void hw_busywait(uint32_t nb_loops) {
    __asm__("   .align 4         \n" // to make it more predictable
            "1: subs  %0, %0, #1 \n" // 1 cycle
            "   bne   1b         \n" // 1+P if taken, 1 otherwise
            :
            : "r"(nb_loops)
            : );
    /* Cycle timing based on 0-wait-state operation. P is pipeline
       refill.  According to datasheet this ranges from 1 to 3
       depending on alignment and width of the target instruction, and
       whether the processor manages to speculate the address
       early. */
}


/* Trough calibration on a scope, we find the loop frequency is 12Mhz
   for 72MHz CPU frequency.  It is not clear how to compute that
   number from what is described in the datasheet. */
INLINE void hw_busywait_us(uint32_t us) { hw_busywait(HW_LOOPS_PER_US * us); }
INLINE void hw_busywait_ms(uint32_t ms) { while(ms--) hw_busywait_us(1); }





/* Hold address and value for time-critical register writes.
   In most cases the compiler will optimize away the use of the struct. */
struct hw_delayed_write {
    uint32_t addr;
    uint32_t val;
};
/* Single immediate write */
INLINE void hw_delayed_write(struct hw_delayed_write dw) {
    MMIO32(dw.addr) = dw.val;
}
/* Single write after busy loop (3 cycles resolution). */
INLINE void hw_delayed_write_busywait(uint32_t nb_busy_loops, struct hw_delayed_write dw) {
    __asm__("1: subs  %0, %0, #1 \n" // 1 cycle
            "   bne   1b         \n" // 2 if taken, 1 otherwise
            "   str   %2, [%1]   \n" // perform delayed write
            :
            : "r"(nb_busy_loops),
              "r"(dw.addr),
              "r"(dw.val)
            : );
}
/* Double write, in order, without gap. */
INLINE void hw_delayed_write_seq2(struct hw_delayed_write dw1, struct hw_delayed_write dw2) {
    __asm__("   str %1, [%0] \n"
            "   str %3, [%2] \n"
            :
            : "r"(dw1.addr), "r"(dw1.val),
              "r"(dw2.addr), "r"(dw2.val)
            : );
}
/* Triple write, in order, without gap. */
INLINE void hw_delayed_write_seq3(struct hw_delayed_write dw1,
                                  struct hw_delayed_write dw2,
                                  struct hw_delayed_write dw3) {
    __asm__("   str %1, [%0] \n"
            "   str %3, [%2] \n"
            "   str %5, [%4] \n"
            :
            : "r"(dw1.addr), "r"(dw1.val),
              "r"(dw2.addr), "r"(dw2.val),
              "r"(dw3.addr), "r"(dw3.val)
            : );
}
/* Double write, in order, without gap, afer busy loop (3 cycles res) */
INLINE void hw_delayed_write_seq2_busywait(uint32_t nb_busy_loops,
                                           struct hw_delayed_write dw1,
                                           struct hw_delayed_write dw2) {
    __asm__("1: subs  %4, %4, #1 \n" // 1 cycle
            "   bne   1b         \n" // 2 if taken, 1 otherwise
            "   str %1, [%0]     \n"
            "   str %3, [%2]     \n"
            : : "r"(dw1.addr), "r"(dw1.val),
                "r"(dw2.addr), "r"(dw2.val),
                "r"(nb_busy_loops) : );
}


/* Peripheral and SRAM bitbanding.
   Note: this works wel with the dev struct: *hw_bitband(&dev.tim2.sr, 0) = 0;
   See STM32F10xxx Cortex-M3 programming manual, section 2.2.5 Bit-banding
*/
INLINE volatile uint32_t hw_bitband_addr(uint32_t addr, uint32_t bit) {
    uint32_t segment = addr & 0xF0000000; // valid for 0x20000000 and 0x40000000
    return segment + 0x02000000 + ((addr & 0x000FFFFF) * 32) + ((bit & 0x3F) * 4);
}
INLINE volatile uint32_t *hw_bitband(volatile uint32_t *ptr, uint32_t bit) {
    uint32_t addr = (uint32_t)ptr;
    return (volatile void*)hw_bitband_addr(addr, bit);
}


/* Write to GPIO CRL / CRH config registers.

   port: A=0,B=1,C=2,...
   pin: 0-15

   Function optimizes away to a short sequence of instructions if all
   arguments are constants.

   This replaces gpio_set_mode() which is very slow (7us).
   See libopencm3/include/libopencm3/stm32/f1/memorymap.h

   config parameter is 4 bits per pin: [ CNF:2 | MODE:2 ]

       __00: Input mode (reset state)
       __01: Output mode, max speed 10 MHz.
       __10: Output mode, max speed 2 MHz.
       __11: Output mode, max speed 50 MHz.

     In input mode:

       00__: Analog mode
       01__: Floating input (reset state)
       10__: Input with pull-up / pull-down
       11__: Reserved

     In output mode:

       _0__: push-pull
       _1__: open-drain
       0___: general purpose output
       1___: Alternate function

   Pull up/down is configured in PxODR port output data register.

*/

// Common uses:                             // CNF:2, MODE:2
#define HW_GPIO_CONFIG_INPUT           0x4  // 0100 gpio input, floating
#define HW_GPIO_CONFIG_INPUT_PULL      0x8  // 1000 gpio input, pullup/down  (ODR=pull_dir)
#define HW_GPIO_CONFIG_OUTPUT          0x3  // 0011 gpio output, push-pull
#define HW_GPIO_CONFIG_OUTPUT_2MHZ     0x2  // 0010 gpio output, push-pull, 2MHz
#define HW_GPIO_CONFIG_ALTFN           0xB  // 1011 alternate function output, 50MHz
#define HW_GPIO_CONFIG_ALTFN_2MHZ      0xA  // 1010 alternate function output, 2MHz
#define HW_GPIO_CONFIG_OPEN_DRAIN_2MHZ 0x6  // 0110 open drain output, 2MHz


INLINE struct hw_delayed_write hw_gpio_config_dw(uint32_t gpio, uint32_t pin, uint32_t config) {
    uint32_t shift = (pin&7) * 4;
    uint32_t offset = 4 * ((pin>>3) & 1); // CRL=0, CRH=4
    uint32_t mask = ~(0xF << shift);
    uint32_t replace = (config & 0xF) << shift;
    struct hw_delayed_write dw = {
        .addr = gpio + offset,
        .val  = (MMIO32(gpio + offset) & mask) | replace};
    return dw;
}
INLINE void hw_gpio_high(uint32_t gpio, uint32_t pin);
INLINE void hw_gpio_low(uint32_t gpio, uint32_t pin);


INLINE void hw_gpio_config(uint32_t gpio, uint32_t pin, uint32_t config) {
    hw_delayed_write(hw_gpio_config_dw(gpio, pin, config));
}
INLINE void hw_gpio_write(uint32_t gpio, uint32_t pin, int value) {
    if (value)
        hw_gpio_high(gpio,pin);
    else
        hw_gpio_low(gpio,pin);
}
INLINE void hw_gpio_pulse_high(uint32_t gpio, uint32_t pin, uint32_t nb_loops) {
    hw_gpio_high(gpio, pin);
    hw_busywait(nb_loops);
    hw_gpio_low(gpio, pin);
}
INLINE void hw_gpio_pulse_low(uint32_t gpio, uint32_t pin, uint32_t nb_loops) {
    hw_gpio_low(gpio, pin);
    hw_busywait(nb_loops);
    hw_gpio_high(gpio, pin);
}
INLINE void hw_gpio_pulse(uint32_t gpio, uint32_t pin, int value, uint32_t nb_loops) {
    if (value)
        hw_gpio_pulse_high(gpio,pin,nb_loops);
    else
        hw_gpio_pulse_low(gpio,pin,nb_loops);
}

// Note: this is likely not correct because of BRR being a 16 bit write
// There is currently no delayed write abstraction for 16 bits registers.
/* /\* delayed write configuration for gpio write operation *\/ */
/* INLINE struct hw_delayed_write hw_gpio_write_dw(uint32_t gpio, uint32_t pin, uint32_t value) { */
/*     struct hw_delayed_write dw = { */
/*         // see libopencm3/stm32/f1/gpio.h, look for offset in GPIO_BSRR, GPIO_BRR */
/*         .addr = gpio + value ? 0x10 : 0x14, */
/*         .val = 1 << pin */
/*     }; */
/*     return dw; */
/* } */

INLINE int hw_gpio_read(uint32_t gpio, uint32_t pin) {
    return (GPIO_IDR(gpio) >> pin) & 1;
}
/* Switch between alternate function (alt=1) and gpo (alt=0) can be
   done with a single bitband write.  Call hw_gpio_config() once first. */
// TODO: test this
INLINE struct hw_delayed_write hw_gpio_config_ag_dw(uint32_t gpio, uint32_t pin, uint32_t alt) {
    uint32_t bit = (pin&7) * 4 + 3;
    uint32_t offset = 4 * ((pin>>3) & 1); // CRL=0, CRH=4
    struct hw_delayed_write dw = {hw_bitband_addr(gpio + offset, bit), alt};
    return dw;
}

/* GPIO config struct. */
struct hw_gpio {
    uint32_t rcc;
    uint32_t gpio;
    uint32_t pin;
};
#define HW_GPIO_NONE {}
#define HW_GPIO_PA0  {RCC_GPIOA, GPIOA,  0}   // TIM2 OC1
#define HW_GPIO_PA1  {RCC_GPIOA, GPIOA,  1}
#define HW_GPIO_PA3  {RCC_GPIOA, GPIOA,  3}
#define HW_GPIO_PA6  {RCC_GPIOA, GPIOA,  6}   // SPI1 TX SLAVE (MISO)
#define HW_GPIO_PA7  {RCC_GPIOA, GPIOA,  7}   // SPI1 RX SLAVE (MOSI)
#define HW_GPIO_PA9  {RCC_GPIOA, GPIOA,  9}   // USART1 TX
#define HW_GPIO_PA10 {RCC_GPIOA, GPIOA, 10}   // USART1 RX
#define HW_GPIO_PB0  {RCC_GPIOB, GPIOB,  0}   // RS485 DIR
#define HW_GPIO_PB4  {RCC_GPIOB, GPIOB,  4}   // dont use: JTAG
#define HW_GPIO_PB14 {RCC_GPIOB, GPIOB, 14}
#define HW_GPIO_PC5  {RCC_GPIOC, GPIOC,  5}
#define HW_GPIO_PC6  {RCC_GPIOC, GPIOC,  6}

#define HW_GPIO(port,pin) {RCC_GPIO##port,GPIO##port,GPIO##pin}


// Convenience
INLINE void hw_gpio_output (struct hw_gpio g) { if (g.gpio) hw_gpio_config(g.gpio, g.pin, HW_GPIO_CONFIG_OUTPUT); }
INLINE void hw_gpio_input  (struct hw_gpio g) { if (g.gpio) hw_gpio_config(g.gpio, g.pin, HW_GPIO_CONFIG_INPUT);  }
INLINE void hw_gpio_altfn  (struct hw_gpio g) { if (g.gpio) hw_gpio_config(g.gpio, g.pin, HW_GPIO_CONFIG_ALTFN);  }
INLINE void hw_gpio_rcc_on (struct hw_gpio g) { if (g.gpio) rcc_periph_clock_enable(g.rcc); }
INLINE void hw_gpio_set (struct hw_gpio g, int val) {
    if (g.gpio) hw_gpio_write(g.gpio, g.pin, val);
}



// DMA configs (see e.g. hw_spi)
struct hw_dma {
    uint32_t rcc_dma;
    uint32_t nvic_irq;
    uint32_t dma;
    uint32_t chan;
};

#define HW_DMA_1_2 { RCC_DMA1, NVIC_DMA1_CHANNEL2_IRQ, DMA1, DMA_CHANNEL2 }
#define HW_DMA_1_3 { RCC_DMA1, NVIC_DMA1_CHANNEL3_IRQ, DMA1, DMA_CHANNEL3 }
#define HW_DMA_1_4 { RCC_DMA1, NVIC_DMA1_CHANNEL4_IRQ, DMA1, DMA_CHANNEL4 }
#define HW_DMA_1_5 { RCC_DMA1, NVIC_DMA1_CHANNEL5_IRQ, DMA1, DMA_CHANNEL5 }




/* ----------------------------------------------------------------------
   hw_app_* : Memory and interrupt setup for app starting from bootloader
   - init: call from start():
   - stop: call from stop():
*/
void init_memory(void);  // normally called from pre_main()
extern uint32_t vector_table;
INLINE void hw_app_init(void) {
    /* Set Vector Table Offset to our memory based vector table */
    SCB_VTOR = (uint32_t)&vector_table;
    /* Do this here as well.  Starting application from bootloader
     * this needs to be done explicitly.  Normally it is called from
     * pre_main(). */
    init_memory();
}
INLINE void hw_app_boot(void *vector_table) {
    /* Update core's vector table register, set stack pointer and
       jump to application entry point. */
    void **v = (void **)vector_table;
    SCB_VTOR = (uint32_t)v;
    __asm__("mov %0, sp \n"
            "bx  %1     \n"
            :
            : "r"(v[0]),
              "r"(v[1])
            : "sp" );

}
INLINE void hw_app_stop(void) {
    /* All IRQs pointing into old code will need to be turned off
       before it is overwritten by the bootloader.  It's easier to
       just turn off everything.  */
    for (uint32_t irq = 0; irq < NVIC_IRQ_COUNT; irq++) {
        nvic_disable_irq(irq);
    }
    /* FIXME: Just trying things out..  Something isn't right in one
       of the apps if TIM2 has been enabled .*/
    SCB_VTOR = 0x8000000;
}






#endif

