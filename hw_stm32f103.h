#ifndef HW_STM32F103_H
#define HW_STM32F103_H

#include "macros.h"

/* This code is based for the larger part on libopencm3 code.
   E.g. hw_usart_enable() is a copy of libopencm3's usart_enable(), etc.. */

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


/* This code provides hardware configuration as inline functions, to
   allow the compiler to better optimize speed and space.

   The basic idea is to separate:

   - configuration: 'static const struct' configuration structures

   - functionality: 'static inline' C functions parameterized by those
                    structures

   - instantiation: wrapper functions in the application's main .c
                    file that combine configuration and functionality



   FIXME: replace macro magic with plain C structs.

   At points where code efficiency is essential, calls to libopencm3
   have been inlined and possibly replaced with bitbanding access.
   The result is simple read, write or read/modify/write instructions.


   Naming scheme for hardware events:

   - init:         called once at startup
   - arm/trigger:  separate (expensive) arming sequence and (cheap) trigger
   - start:        arm then trigger (in case it doesn't make sense to separate)
   - ack:          acknowledge event from ISR

   Each event has associated ISRs which use the standard _isr() names
   from libopencm3.  Where necessary, there are macros to abstract
   over these, e.g. HW_TIM_ISR().


   Documentation: processor, peripherals and (2x) package/pin info.

   -rw-r--r-- 1 tom tom  4199056 Jan 27 10:22 STM32F103 Programming Manual.pdf
   -rw-r--r-- 1 tom tom 13016697 Jan 27 10:22 STM32F103 Reference Manual.pdf
   -rw-r--r-- 1 tom tom  1697666 Jan 27 10:22 STM32F103x8.pdf
   -rw-r--r-- 1 tom tom  1973696 Jan 27 10:22 STM32F103xD.pdf

*/

/* Parameterization of generic STM32F code */
#define HW_LOOPS_PER_US 12
#define HW_USB_DRIVER &st_usbfs_v1_usb_driver


/* Generic code, shared between F103 and F407 */
#include "hw_stm32f_common.h"

#include <libopencm3/stm32/f1/rcc.h>
#include <libopencm3/stm32/f1/timer.h>
#include <libopencm3/stm32/f1/gpio.h>
#include <libopencm3/stm32/f1/usart.h>
#include <libopencm3/stm32/f1/spi.h>




#include "hw_stm32f103_rcc.h"  // inline version of libopencm3/lib/stm32/f1/rcc.c


/* Register struct overlay. */
//#include "registers_stm32f103.h"

#include <stdint.h>




/* Map hw.h (e.g. SPI1_MASTER config name to libopencm3 isr name. */
#define HW_TIM_ISR(X) CONCAT(tim, CONCAT(X,_isr))  // 2 -> tim_2_isr

#define HW_USART_ISR(X) CONCAT(usart, CONCAT(X,_isr))

#define HW_DMA_ISR_TX(X)  CONCAT(HW_DMA_ISR_SPI,CONCAT(X,_TX))
#define HW_DMA_ISR_RX(X)  CONCAT(HW_DMA_ISR_SPI,CONCAT(X,_RX))

/* Why CONCAT?
   The macro-expansion below is usartPORT_isr
#define ISR(n) usart##n##_isr
#define PORT 3
ISR(PORT)
*/


#define HW_DMA_ISR_SPI1_RX  dma1_channel2_isr
#define HW_DMA_ISR_SPI1_TX  dma1_channel3_isr
#define HW_DMA_ISR_SPI2_RX  dma1_channel4_isr
#define HW_DMA_ISR_SPI2_TX  dma1_channel5_isr


/* Gpio */
INLINE void hw_gpio_high(uint32_t gpio, uint32_t pin) {
    GPIO_BSRR(gpio) = 1 << pin;
}
INLINE void hw_gpio_low(uint32_t gpio, uint32_t pin) {
    GPIO_BRR(gpio) = 1 << pin;
}
INLINE void hw_gpio_toggle(uint32_t gpio, uint32_t pin) {
    GPIO_ODR(gpio) = GPIO_IDR(gpio) ^ (1 << pin);
}

/* Write bit using BSRR only. */
INLINE void hw_gpio_write_v2(uint32_t gpio, uint32_t pin, uint32_t val) {
    pin += 16 * (!val);
    GPIO_BSRR(gpio) = 1 << pin;
}
/* Write 1 to the address to set/reset the pin.  These are convenient
   from ISR for fast response, requiring only three instructions:
   pc-relative bitband address load, literal 1 register load, and
   store to the bitband address. */
INLINE
__attribute__((const))
volatile uint32_t *hw_gpio_high_bitband(uint32_t gpio, uint32_t pin) {
    return hw_bitband((void*)&GPIO_BSRR(gpio), pin);
}
INLINE
__attribute__((const))
volatile uint32_t *hw_gpio_low_bitband(uint32_t gpio, uint32_t pin) {
    return hw_bitband((void*)&GPIO_BRR(gpio), pin);
}

/* Macro versions that can be used in const initializers. */
#define HW_GPIO_HIGH_BITBAND(gpio,pin) \
    HW_BITBAND((void*)&GPIO_BSRR(gpio), pin)

#define HW_GPIO_LOW_BITBAND(gpio,pin) \
    HW_BITBAND((void*)&GPIO_BRR(gpio), pin)


/* Timers */
void hw_tim_stretch_14_to_15(volatile uint32_t* tim_arr);

INLINE void hw_tim_clear_if(uint32_t tim) {
    *hw_bitband(&(TIM_SR(tim)), 0) = 0;
}
INLINE void hw_tim_wait_if(uint32_t tim) {
    while(*hw_bitband(&(TIM_SR(tim)), 0) == 0);
};
INLINE void hw_tim_sync(uint32_t tim) {
    hw_tim_clear_if(tim);
    hw_tim_wait_if(tim);
}
INLINE void hw_tim_disable_counter(uint32_t tim) {
    TIM_CR1(tim) &= ~TIM_CR1_CEN;
}
INLINE void hw_tim_enable_counter(uint32_t tim) {
    TIM_CR1(tim) |= TIM_CR1_CEN;
}
INLINE void hw_tim_set_period(uint32_t tim, uint32_t period) {
    TIM_ARR(tim) = period;
}
INLINE void hw_tim_set_ccr1(uint32_t tim, uint32_t value) {
    TIM_CCR1(tim) = value;
}

INLINE void hw_tim_set_counter(uint32_t tim, uint32_t count) {
    TIM_CNT(tim) = count;
}
INLINE uint16_t hw_tim_counter(uint32_t tim) {
    return TIM_CNT(tim);
}
INLINE void hw_tim_set_prescaler(uint32_t tim, uint16_t psc) {
    TIM_PSC(tim) = psc;
}


INLINE void hw_reset(void) {
    SCB_AIRCR = 0x05FA0004;
}


/* Peripheral reset */
#define _RCC_REG(i)  MMIO32(RCC_BASE + ((i) >> 5))
#define _RCC_BIT(i)  (1 << ((i) & 0x1f))

INLINE void hw_rcc_periph_off(enum rcc_periph_rst rst) {
    _RCC_REG(rst) |= _RCC_BIT(rst);
}
INLINE void hw_rcc_periph_on(enum rcc_periph_rst rst) {
    _RCC_REG(rst) &= ~_RCC_BIT(rst);
}
INLINE void hw_rcc_periph_reset_pulse(enum rcc_periph_rst rst) {
    hw_rcc_periph_off(rst);
    hw_busywait(10);
    //hw_busywait(5);
    hw_rcc_periph_on(rst);
}

/* Cycle accurate busy wait. */
// FIXME: not used nor verified.
void hw_busywait_core(uint32_t nb_loops);
INLINE void hw_busywait_cycles(uint32_t nb_ticks) {
    hw_busywait_core(nb_ticks / 4); // + offset
}








/* ---------------------------------------------
   hw_20kHz_* : TIM2,3,4 for periodic interrupts
   - init: call once
   - ack:  call from timX_isr()
*/
struct hw_periodic {
    uint32_t rcc;   // peripheral clock
    uint32_t irq;   // interrupt request
    uint32_t tim;   // timer instance
    uint32_t div;
    uint32_t pre;   // prescaler setting
};


INLINE void hw_periodic_init(struct hw_periodic c) {
    rcc_periph_clock_enable(c.rcc);
    //nvic_set_priority(c.irq, 1);
    nvic_enable_irq(c.irq);

    // Setup tim for periodic clock (only xD has TIM5,6!)
    rcc_periph_reset_pulse(c.tim);
    timer_set_mode(c.tim, TIM_CR1_CKD_CK_INT,TIM_CR1_CMS_EDGE,TIM_CR1_DIR_UP);
    timer_set_prescaler(c.tim, c.pre-1); // 72MHz or 36MHz in depending on bus
    hw_tim_set_period(c.tim, c.div-1); // -> periodic frequency
    timer_continuous_mode(c.tim);
    hw_tim_set_counter(c.tim, 0);
    timer_enable_counter(c.tim);
    timer_enable_irq(c.tim, TIM_DIER_UIE);
}
INLINE void hw_periodic_ack(struct hw_periodic c) {
    //TIM_SR(tim) &= ~TIM_SR_UIF; /* Clear interrrupt flag. */
    *hw_bitband(&(TIM_SR(c.tim)), 0) = 0;
}
INLINE void hw_periodic_disable(struct hw_periodic c) {
    nvic_disable_irq(c.irq);
    rcc_periph_reset_pulse(c.tim);
}



/* ---------------------------------------------------------
   hw_delay_* : TIM2,3,4 for interrupt-based one-shot delays
   - init:    call once before start
   - arm:     set up timer registers, leave disabled
   - trigger: start countdown
   - ack:     call from timX_isr()
*/

struct hw_delay {
    uint32_t rcc;   // peripheral clock
    uint32_t irq;   // interrupt request
    uint32_t tim;   // timer instance
    uint32_t psc;   // prescaler (backwards compat set to 0)
};



INLINE void hw_delay_init(struct hw_delay c, uint32_t default_ticks, int interrupt) {
    rcc_periph_clock_enable(c.rcc);
    if (interrupt) {
        //nvic_set_priority(c.irq, 1);
        nvic_enable_irq(c.irq);
    }
    rcc_periph_reset_pulse(c.tim);
    hw_tim_disable_counter(c.tim);
    timer_set_mode(c.tim, TIM_CR1_CKD_CK_INT,TIM_CR1_CMS_EDGE,TIM_CR1_DIR_UP);
    timer_set_master_mode(c.tim, TIM_CR2_MMS_COMPARE_OC1REF); // CR2.MMS = 1xx -> OCxREF = TRGO
    timer_set_prescaler(c.tim, c.psc); // 72MHz in
    hw_tim_set_period(c.tim, default_ticks); // for arm_fixed
    timer_enable_irq(c.tim, TIM_DIER_UIE);
    timer_one_shot_mode(c.tim);
}
INLINE void hw_delay_arm_fixed(struct hw_delay c) {
    // TIM_CR1(c.tim) &= ~TIM_CR1_CEN;  // assume already disabled
    TIM_CNT(c.tim) = 0;
}
INLINE void hw_delay_arm(struct hw_delay c, uint32_t ticks) {
    // hw_tim_disable_counter(c.tim);
    hw_tim_set_period(c.tim, ticks);
    hw_tim_set_counter(c.tim, 0);
}
INLINE void hw_delay_trigger(struct hw_delay c) {
    // timer_enble_counter(TIMx); // call
    TIM_CR1(c.tim) |= TIM_CR1_CEN; // inlined
}
INLINE void hw_delay_trigger_fixed(struct hw_delay c) {
    hw_delay_arm_fixed(c);
    hw_delay_trigger(c);
}
INLINE void hw_delay_ack(struct hw_delay c) {
    hw_tim_disable_counter(c.tim);
    TIM_SR(c.tim) &= ~TIM_SR_UIF; /* Clear interrrupt flag. */
}
INLINE void hw_delay_disable_counter(struct hw_delay c) {
    hw_tim_disable_counter(c.tim);
}
INLINE void hw_delay_disable_interrupt(struct hw_delay c) {
    timer_disable_irq(c.tim, TIM_DIER_UIE);
    hw_delay_ack(c);
}
INLINE uint32_t hw_delay_poll(struct hw_delay c) {
    return TIM_SR(c.tim) & TIM_SR_UIF;
}
INLINE void hw_delay_poll_update(struct hw_delay c, uint32_t *ticks) {
    if (hw_delay_poll(c)) {
        (*ticks)++;
        hw_delay_ack(c);
        hw_delay_trigger_fixed(c);
    }
}

/* FIXME: works up to us=910 */
INLINE void hw_delay_us(struct hw_delay c, uint32_t us) {
    hw_delay_arm(c, 72 * us);
    hw_delay_trigger(c);
    while (!hw_delay_poll(c));
    hw_delay_ack(c);
}


/* -------------------------------------
   hw_gate_* : TIM2,3,4 for gated delays
   - init:    call once before start
   - arm:     prepare trigger
   - trigger: start countdown
*/

struct hw_gate {
    uint32_t rcc;   // peripheral clock
    uint32_t tim;   // timer instance
    uint32_t itr;   // use other timer as triger input
};

INLINE void hw_gate_init(struct hw_gate c) {
    rcc_periph_clock_enable(c.rcc);
    rcc_periph_reset_pulse(c.tim);
    hw_tim_disable_counter(c.tim);
    /* It seems that setting the polarity with CC1P does not affect
       the value that is sent to TRGO, so have counter count down
       instead. */
    timer_set_mode(c.tim, TIM_CR1_CKD_CK_INT, TIM_CR1_CMS_EDGE, TIM_CR1_DIR_DOWN);

    /* Trigger out is OC1 output with polarity such that out=1 if counter < CCR1
       This will be used to gate the other timer. */
    timer_set_master_mode(c.tim, TIM_CR2_MMS_COMPARE_OC1REF); // CR2.MMS = 1xx -> OCxREF = TRGO
    timer_set_oc_mode(c.tim, TIM_OC1, TIM_OCM_PWM2);

    /* We use the timer we're gating to clock us, so our period is the
     * number of pulses to generate. */
    timer_set_prescaler(c.tim, 0);
    timer_slave_set_mode(c.tim, TIM_SMCR_SMS_ECM1); // Trigger on TRGI
    timer_slave_set_trigger(c.tim, c.itr); // ITR -> TRGI

    timer_one_shot_mode(c.tim);
}
INLINE void hw_gate_arm(struct hw_gate c, uint32_t ticks) {
    hw_tim_disable_counter(c.tim);
    hw_tim_set_period(c.tim, 0xFFFF); // not used: we'll disable it before it runs out
    hw_tim_set_ccr1(c.tim, 0xFFFF - ticks);
    hw_tim_set_counter(c.tim, 0xFFFF);  // gate is now open
    hw_tim_enable_counter(c.tim); // we now count other oscillator timer's update events
}
INLINE struct hw_delayed_write hw_gate_trigger_dw(struct hw_gate c) {
    struct hw_delayed_write dw = {(uint32_t)hw_bitband(&TIM_CR1(c.tim), 0), 1};
    return dw;
}
INLINE void hw_gate_trigger(struct hw_gate c) {
    hw_delayed_write(hw_gate_trigger_dw(c));
}


/* -------------------------------------------
   hw_clockgen_* : external clock generaration
   - init:    call once before start
   - arm:     set up, wait for trigger
   - trigger  start clocking
   - disable: stop signal
*/
struct hw_clockgen {
    uint32_t rcc_tim, rcc_gpio; // peripheral clocks
    uint32_t tim;               // timer instance
    uint32_t gpio, pin;         // gpio port, pin for chan output
    uint32_t div;               // timer freq = 72MHz / clockdiv
    uint32_t duty;              // duty cycle (same units).
    uint32_t phase;             // reset phase
    uint32_t pol;
    uint32_t chan;              // OC channel
    int32_t gate_itr;           // if >= 0 this is a gated timer
    uint32_t irq;               // if non-zero this is the interrupt
};




// PA9 has both TIM1_CH2 and USART1_TX.  When USART is on, TIM1 needs
// to be reset (turning RCC off doesn't have the desired effect;
// tim_reset strobes the bit in RCC_REG.)
INLINE void hw_clockgen_off(struct hw_clockgen c) {
    hw_tim_disable_counter(c.tim);
    rcc_periph_reset_pulse(c.tim);
    //rcc_periph_clock_disable(c.rcc_tim);
}


INLINE void hw_clockgen_init(struct hw_clockgen c) {
    rcc_periph_clock_enable(c.rcc_tim);
    rcc_periph_clock_enable(c.rcc_gpio);

    /* Pin is driven high when clock is not running. */
    hw_gpio_high(c.gpio,c.pin);
    hw_gpio_config(c.gpio,c.pin,HW_GPIO_CONFIG_OUTPUT);

    /* Configure PWM, period = c.clockdiv, mid = c.clockdiv / 2. */
    rcc_periph_reset_pulse(c.tim);
    timer_set_oc_mode(c.tim, c.chan, TIM_OCM_PWM2);
    timer_enable_oc_output(c.tim, c.chan);

    /* c.pol is the initial output if counter==0 */
    if (c.pol) {
        timer_set_oc_polarity_low(c.tim, c.chan);
    }
    else {
        timer_set_oc_polarity_high(c.tim, c.chan);
    }


    /* Note that for the advanced timers the break functionality must
       be enabled before the signal will appear at the output, even
       though break is not being used. */
    timer_enable_break_main_output(c.tim);

    /* Allow update to trigger slave. */
    timer_set_master_mode(c.tim, TIM_CR2_MMS_UPDATE);

    /* Allow gating */
    if (c.gate_itr >= 0) {
        // is this compatible with TIM_CR2_MMS_UPDATE?
        timer_slave_set_mode(c.tim, TIM_SMCR_SMS_GM); // Gated mode
        timer_slave_set_trigger(c.tim, c.gate_itr); // ITR -> TRGI
    }

    timer_set_oc_value(c.tim, c.chan, c.duty);
    hw_tim_set_period(c.tim, c.div-1);
    timer_enable_preload(c.tim); // load from ARR on UE

    if (c.irq) {
        //nvic_set_priority(c.irq, 1);
        nvic_enable_irq(c.irq);
        timer_enable_irq(c.tim, TIM_DIER_UIE);
    }
}

// B14 SPI2_MISO
// B13 SPI2_CLK
// C6  TIM3_CH1

INLINE void hw_clockgen_arm(struct hw_clockgen c) {
    /* phase_offset tweak >=0 has output start at 1.  Else 0. */
    hw_tim_disable_counter(c.tim);
    hw_tim_set_counter(c.tim, c.phase);
    hw_gpio_config(c.gpio,c.pin,HW_GPIO_CONFIG_ALTFN); // altfun, pushpull, 50MHz
}
INLINE struct hw_delayed_write hw_clockgen_trigger_dw(struct hw_clockgen c) {
    /// see timer_enable_counter(tim);
    /// use bit-banding instead
    struct hw_delayed_write dw = {(uint32_t)hw_bitband(&TIM_CR1(c.tim), 0), 1};
    return dw;
}
INLINE void hw_clockgen_trigger(struct hw_clockgen c) {
    hw_delayed_write(hw_clockgen_trigger_dw(c));
}
INLINE void hw_clockgen_init_arm_trigger(struct hw_clockgen c) {
    hw_clockgen_init(c);
    hw_clockgen_arm(c);
    hw_clockgen_trigger(c);
}


INLINE struct hw_delayed_write hw_clockgen_disable_dw(struct hw_clockgen c) {
    // Leave counter running.  Disengage timer from pin instead.
    return hw_gpio_config_dw(c.gpio,c.pin,HW_GPIO_CONFIG_OUTPUT);
}
INLINE void hw_clockgen_disable(struct hw_clockgen c) {
    hw_delayed_write(hw_clockgen_disable_dw(c));
}

INLINE void hw_clockgen_wait(struct hw_clockgen c) {
    while (!*hw_bitband(&(TIM_SR(c.tim)), 0)); // wait for IF
    *hw_bitband(&(TIM_SR(c.tim)), 0) = 0; // clear IF
}

INLINE void hw_clockgen_ack(struct hw_clockgen c) {
    //TIM_SR(tim) &= ~TIM_SR_UIF; /* Clear interrrupt flag. */
    *hw_bitband(&(TIM_SR(c.tim)), 0) = 0;
}
INLINE void hw_clockgen_duty(struct hw_clockgen c, uint32_t value) {
    /* Copied from timer_common.all.c to allow inline. */
    // timer_set_oc_value(c.tim, c.chan, c.duty);
    switch (c.chan) {
    case TIM_OC1:
        TIM_CCR1(c.tim) = value;
        break;
    case TIM_OC2:
        TIM_CCR2(c.tim) = value;
        break;
    case TIM_OC3:
        TIM_CCR3(c.tim) = value;
        break;
    case TIM_OC4:
        TIM_CCR4(c.tim) = value;
        break;
    }

}


/* -------------------------------------------
   hw_multi_pwm_* : Multi-channel PWM
   - init:    call once before start
   - arm:     set up, wait for trigger
   - trigger  start clocking
   - disable: stop signal
*/
struct hw_multi_pwm {
    uint32_t rcc_tim;        // peripheral clock
    uint32_t tim;            // timer instance
    struct hw_gpio gpio[4];  // io pins
    uint32_t gpio_config;    // GPIO mode (eg. HW_GPIO_CONFIG_ALTFN)
    uint32_t div;            // timer freq = 72MHz / clockdiv
    uint32_t duty;           // initial duty cycle (libopencm3 enum)
    uint32_t irq;            // if non-zero this is the interrupt
};

INLINE void hw_multi_pwm_init(struct hw_multi_pwm c) {
    rcc_periph_clock_enable(c.rcc_tim);
    rcc_periph_reset_pulse(c.tim);
    for(int i=0; i<4; i++) {
        enum tim_oc_id channel[] = {TIM_OC1,TIM_OC2,TIM_OC3,TIM_OC4};
        if (c.gpio[i].gpio) {
            rcc_periph_clock_enable(c.gpio[i].rcc);
            hw_gpio_config(c.gpio[i].gpio, c.gpio[i].pin, c.gpio_config);
            /* Configure PWM, period = c.clockdiv, mid = c.clockdiv / 2. */
            timer_set_oc_mode(c.tim, channel[i], TIM_OCM_PWM2);
            timer_enable_oc_output(c.tim, channel[i]);
            timer_set_oc_polarity_low(c.tim, channel[i]);

            /* Note that for the advanced timers the break
               functionality must be enabled before the signal will
               appear at the output, even though break is not being
               used. */
            timer_enable_break_main_output(c.tim);

            timer_set_oc_value(c.tim, channel[i], c.duty);

            timer_enable_oc_preload(c.tim, channel[i]);
        }
    }
    hw_tim_set_period(c.tim, c.div-1);
    hw_tim_set_counter(c.tim, 0);
    timer_enable_preload(c.tim); // load from ARR on UE
    //TIM_CCMR1_OC1PE
    if (c.irq) {
        nvic_enable_irq(c.irq);
        timer_enable_irq(c.tim, TIM_DIER_UIE);
    }
}
INLINE void hw_multi_pwm_start(struct hw_multi_pwm c) {
    hw_tim_enable_counter(c.tim);
}
INLINE void hw_multi_pwm_stop(struct hw_multi_pwm c) {
    hw_tim_disable_counter(c.tim);
}
INLINE void hw_multi_pwm_ack(struct hw_multi_pwm c) {
    //TIM_SR(tim) &= ~TIM_SR_UIF; /* Clear interrrupt flag. */
    *hw_bitband(&(TIM_SR(c.tim)), 0) = 0;
}


// CHANNEL IS NOT LIBOPENCM3 ENUM!
INLINE void hw_multi_pwm_duty(struct hw_multi_pwm c, uint32_t channel, uint32_t value) {
    switch (channel) {
    case 0: TIM_CCR1(c.tim) = value; break;
    case 1: TIM_CCR2(c.tim) = value; break;
    case 2: TIM_CCR3(c.tim) = value; break;
    case 3: TIM_CCR4(c.tim) = value; break;
    }
}





/* -------------------------------------------------------
   hw_capture_* : Capture counter value on 1->0 transition
   - init:  call one before start
   - arm:   call from anywhere, arms trigger.  isr triggers on event.
   - ack:   call from isr, acknowledges trigger
*/
struct hw_capture {
    uint32_t rcc_tim, rcc_gpio; // peripheral clocks
    uint32_t irq;               // interrupt request
    uint32_t tim;               // timer instance
    uint32_t pre;
    uint32_t div;
    uint32_t ckd;               // sampling clock divider
};

struct hw_capture_channel {
    uint32_t tim;
    uint32_t chan;              // capture / input channel (0-based addressing!)
    uint32_t gpio, pin;         // gpio port, pin for capture source
    uint32_t config;            // CCMR 8bit config slice for channel
    uint32_t pol;               // polarity, 1=active low
};

/* Input capture is very flexible.  There are 3 levels:
   - t: Timer Peripheral
   - c: Channel (capture register)
   - i: Input selector per channel (not all combinations ar possible)

   The inputs refer to pins marked TIM<t>_CH<i> pins.  See reference
   manual for registers TIMx_CCMR1,2 and libopencm code comments for
   timer_ic_set_input().
*/


// init
    // Select the active input: TI4 -> IC3 : TIMxCCMR2 CC3S
    // Program input filter
    // Select edge
    // Program input prescaler
    // Enable capture
    // Disable interrupt,DMA

    // Same for TI4 -> IC4

// poll
    // Check CC3IF, CC4IF
    // Read TIMx_CCR3,4
    // Check overflow + reset if needed


INLINE void hw_capture_init(struct hw_capture c) {
    rcc_periph_clock_enable(c.rcc_tim);
    rcc_periph_clock_enable(c.rcc_gpio);
    if (c.irq) {
        //nvic_set_priority(c.irq, 1);
        nvic_enable_irq(c.irq);
    }

    rcc_periph_reset_pulse(c.tim);
    hw_tim_disable_counter(c.tim);
    timer_set_mode(c.tim, c.ckd,TIM_CR1_CMS_EDGE,TIM_CR1_DIR_UP);
    timer_set_prescaler(c.tim, c.pre-1); // 72 MHz in
    hw_tim_set_period(c.tim, c.div-1);
    timer_continuous_mode(c.tim);
    timer_enable_counter(c.tim);
}
INLINE void hw_capture_channel_init(struct hw_capture_channel c) {

    hw_gpio_config(c.gpio, c.pin, HW_GPIO_CONFIG_INPUT);


    /* Input compare setup in libopencm3 is too complex.
       This is set up from the hints in the reference manual, section
       15.3.5 RM0008 Input capture mode.

       These are the registers to access:
       - init
         - TIMx_CCMR1,2: input source, filter, prescaler
         - TIMx_CCER: polarity, enable channel,
         - TIMx_DIER: enable channel interrupt
       - isr
         - TIMx_CCR1: capture value
         - TIMx_SR: clear CC1IF
    */


    // a CCMR config slice is 8 bits:
    // ICyF:4 | ICyPCS:2 | CCyS:2
    // ICyF   is input capture filter, 0000 = off
    // ICyPCS is prescaler, 00 = no prescaler
    // CCyS   is capture compare selection,
    //    01 means chan_index == input_index
    //    10 means chan_index == input_index ^ 1 (other)

    // pick CCMR1/CCMR2 and shift based on high,low bits of chan.
    volatile uint32_t *tim_ccmr = (&TIM_CCMR1(c.tim)) + ((c.chan >> 1) & 1);
    uint32_t shift = 8 * (c.chan & 1);
    *tim_ccmr = (*tim_ccmr & ~(0xFF << shift)) | (c.config << shift);


    // TIMxCCER has 4 x 4bit slot.
    // res:2 CCyP:1 CCyE
    // CCyP: polarity  1 = active low
    // CCyE: enable
    TIM_CCER(c.tim) |= (c.pol << ((c.chan * 4) + 1));

    // enable channel interrupt
    TIM_DIER(c.tim) |= (1 << (c.chan + 1));
}

#define CCS_SAME  1  // e.g. TI4 -> IC4
#define CCS_OTHER 2  // e.g. TI4 -> IC3

INLINE void hw_capture_channel_arm(struct hw_capture_channel c) {
    hw_gpio_config(c.gpio, c.pin, HW_GPIO_CONFIG_INPUT);
    (&TIM_CCR1(c.tim))[c.chan]; // clears interrupt flag
    TIM_CCER(c.tim) |= (1 << (c.chan * 4)); // enable capture
}
INLINE uint32_t hw_capture_t_now(struct hw_capture c) {
    return TIM_CNT(c.tim);
}
INLINE uint32_t hw_capture_channel_t_event(struct hw_capture_channel c) {
    return (&TIM_CCR1(c.tim))[c.chan]; // clears interrupt flag
}
INLINE void hw_capture_channel_disable(struct hw_capture_channel c) {
    TIM_CCER(c.tim) &= ~(1 << (c.chan * 4)); // disable capture
}
INLINE uint32_t hw_capture_channel_ccif(struct hw_capture_channel c) {
    return (TIM_SR(c.tim) >> (c.chan + 1)) & 1; // capture compare interrupt
}
INLINE uint32_t hw_capture_channel_ccof(struct hw_capture_channel c) {
    return (TIM_SR(c.tim) >> (c.chan + 9)) & 1; // capture compare overflow
}



// MODE 1: Idle=0, 0->1 is sampling edge (first edge).
#define HW_SPI_0RW                              \
    (SPI_CR1_CPOL_CLK_TO_0_WHEN_IDLE |          \
     SPI_CR1_CPHA_CLK_TRANSITION_1)

// MODE 3: Idle=1 0->1 is sampling edge (second edge).
#define HW_SPI_1WR                              \
    (SPI_CR1_CPOL_CLK_TO_1_WHEN_IDLE |          \
     SPI_CR1_CPHA_CLK_TRANSITION_2)




/* ---------------------------------------------------------------------
   hw_spi_nodma_* : bi-directional SPI polling transfers
*/
struct hw_spi_nodma {
    uint32_t rcc_gpio;
    uint32_t rcc_spi;
    uint32_t spi;
    uint32_t rst;
    uint32_t gpio;
    uint32_t out; // pin
    uint32_t in;  // pin
    uint32_t sck; // pin
    uint32_t mode;
    uint32_t div; // 0 == SPI_CR1_BAUDRATE_FPCLK_DIV_2
};

INLINE void hw_spi_nodma_reset(struct hw_spi_nodma c) {

    // During reset pulse, SPI data pin will be glitching and floating.
    // emu_dimod uses hw_spi_set_high() before reset
    hw_rcc_periph_reset_pulse(c.rst);

    uint32_t cr1 =
           c.mode
        |  SPI_CR1_DFF_8BIT
        |  SPI_CR1_MSBFIRST;

    // Only support master mode.
    // The DIV_2 relates to fPCLK, the bus clock of the peripheral:
    // APB1: 36MHz  SPI2,SPI3
    // APB2: 72MHz  SPI1
    hw_gpio_config(c.gpio, c.sck,  HW_GPIO_CONFIG_ALTFN);
    cr1 |= SPI_CR1_MSTR
        |  SPI_CR1_SSM // software NSS
        |  SPI_CR1_SSI
        |  c.div;


    hw_gpio_high(c.gpio, c.out);
    hw_gpio_config(c.gpio, c.out, HW_GPIO_CONFIG_ALTFN);
    hw_gpio_config(c.gpio, c.in,  HW_GPIO_CONFIG_INPUT);

    SPI_CR1(c.spi) = cr1 | SPI_CR1_SPE; // enable
}
INLINE void hw_spi_nodma_init(struct hw_spi_nodma c) {
    // shared between master and slave
    rcc_periph_clock_enable(c.rcc_gpio);
    rcc_periph_clock_enable(c.rcc_spi);
    hw_spi_nodma_reset(c);
}
INLINE int hw_spi_nodma_wr_ready(struct hw_spi_nodma c) {
    return !!(SPI_SR(c.spi) & SPI_SR_TXE);
}
INLINE void hw_spi_nodma_wr_no_wait(struct hw_spi_nodma c, uint8_t byte) {
    SPI_DR(c.spi) = byte;
}
INLINE void hw_spi_nodma_wr(struct hw_spi_nodma c, uint8_t byte) {
    while (!hw_spi_nodma_wr_ready(c));
    hw_spi_nodma_wr_no_wait(c, byte);
}
INLINE int hw_spi_nodma_rd_ready(struct hw_spi_nodma c) {
    return !!(SPI_SR(c.spi) & SPI_SR_RXNE);
}
INLINE uint8_t hw_spi_nodma_rd_no_wait(struct hw_spi_nodma c) {
    return SPI_DR(c.spi);
}
INLINE uint8_t hw_spi_nodma_rd(struct hw_spi_nodma c) {
    while (!hw_spi_nodma_rd_ready(c));
    return hw_spi_nodma_rd_no_wait(c);
}
INLINE uint8_t hw_spi_nodma_rdwr(struct hw_spi_nodma c, uint8_t byte) {
    hw_spi_nodma_wr(c, byte);
    return hw_spi_nodma_rd(c);
}
INLINE void hw_spi_nodma_end(struct hw_spi_nodma c) {
    /* Wait to transmit last data */
    while (!(SPI_SR(c.spi) & SPI_SR_TXE));
    /* Wait until not busy */
    while (SPI_SR(c.spi) & SPI_SR_BSY);
}


/* ---------------------------------------------------------------------
   hw_spi1 : uni-directional SPI DMA transfers
   - init:   one time initialization, call before start
   - start:  start transfer, call from anywhere
   - ack:    call from dma1_channel3_isr()
*/

/* Table 78. Summary of DMA1 requests for each channel:
   2 - SPI1_RX
   3 - SPI1_TX
   4 - SPI2_RX
   5 - SPI2_TX */

struct hw_spi {
    uint32_t rcc_gpio;
    uint32_t rcc_spi;
    uint32_t spi;
    uint32_t rst;
    uint32_t gpio;
    uint32_t data; // pin
    uint32_t sck; // pin
    uint32_t master; // 1=master, 0=slave
    uint32_t tx; // 1=tx mode, 0=rx mode   (no bidir in this config)
    struct hw_dma d;
    uint32_t ie; // interrupt enable
    uint32_t bits;
    uint32_t mode;
    uint32_t div; // 0 == SPI_CR1_BAUDRATE_FPCLK_DIV_2
};



/* For 6MHz bus the first data edge needs to be controlled.
   hw_spi_start() will otherwise set the first data bit before the
   first clock pulse arrives. */
INLINE struct hw_delayed_write hw_spi_data_release_dw(struct hw_spi c) {
    //return hw_gpio_config_dw(c.gpio, c.data, HW_GPIO_CONFIG_ALTFN);
    return hw_gpio_config_ag_dw(c.gpio, c.data, /*alt*/1);
}
INLINE struct hw_delayed_write hw_spi_data_hold_dw(struct hw_spi c) {
    // return hw_gpio_config_dw(c.gpio, c.data, HW_GPIO_CONFIG_OUTPUT);
    return hw_gpio_config_ag_dw(c.gpio, c.data, /*alt*/0);
}
INLINE void hw_spi_data_hold(struct hw_spi c) {
    hw_delayed_write(hw_spi_data_hold_dw(c));
}

// FIXME: during the reset pulse for spi
INLINE void hw_spi_high(struct hw_spi c) {
    hw_gpio_high(c.gpio, c.data);
    hw_gpio_config(c.gpio, c.data, HW_GPIO_CONFIG_OUTPUT);
}


INLINE void hw_spi_reinit(struct hw_spi c) {

    // FIXME: Make pin speed configurable.
    uint32_t altfn =  HW_GPIO_CONFIG_ALTFN;
    //uint32_t altfn =  HW_GPIO_CONFIG_ALTFN_2MHZ;

    uint32_t cr1 =
        c.mode |
        (c.bits == 16 ? SPI_CR1_DFF_16BIT : SPI_CR1_DFF_8BIT) |
        SPI_CR1_MSBFIRST;

    if (c.master) {
        if (c.gpio) {
            hw_gpio_config(c.gpio, c.sck, altfn);
        }

        cr1 |= SPI_CR1_MSTR
            |  SPI_CR1_SSM // software NSS
            |  SPI_CR1_SSI
            |  c.div;
    }
    else {
        if (c.gpio) {
            hw_gpio_config(c.gpio, c.sck,  HW_GPIO_CONFIG_INPUT);
        }

        cr1 &= ~(SPI_CR1_MSTR    // slave mode
               | SPI_CR1_SSI);   // nss low
        cr1 |= SPI_CR1_SSM;      // software slave management
    }
    if (c.tx) {
        if (c.gpio) {
            hw_gpio_high(c.gpio, c.data);
            // Note: this had: FIXME: not ALTFN! - hardcoded for emu_dimod.
            // This should be fixed in the downstream firmware.
            // This really should be ALTFN.  Spent half an hour debugging...
            // hw_gpio_config(c.gpio, c.data, HW_GPIO_CONFIG_OUTPUT);
            hw_gpio_config(c.gpio, c.data, altfn);
            /* After this it's possible to switch switch alt / gpo=1 using
               bitband write in hw_gpio_config_ag_dw(). */
        }
    }
    else {
        cr1 |= SPI_CR1_RXONLY;
        if (c.gpio) {
            hw_gpio_config(c.gpio, c.data, HW_GPIO_CONFIG_INPUT);
        }
    }
    SPI_CR1(c.spi) = cr1 | SPI_CR1_SPE; // enable
}

INLINE void hw_spi_reset(struct hw_spi c) {

    // During reset pulse, SPI data pin will be glitching and floating.
    // emu_dimod uses hw_spi_set_high() before reset
    hw_rcc_periph_reset_pulse(c.rst);
    hw_spi_reinit(c);
}



INLINE void hw_spi_init(struct hw_spi c) {
    // shared between master and slave
    rcc_periph_clock_enable(c.rcc_gpio);
    rcc_periph_clock_enable(c.rcc_spi);
    rcc_periph_clock_enable(c.d.rcc_dma);

    if (c.ie) {
        //nvic_set_priority(c.d.nvic_irq, 1);
        nvic_enable_irq(c.d.nvic_irq);
    }

    hw_spi_reset(c);
}

INLINE void hw_spi_start(struct hw_spi c,
                         const void *data, uint32_t nb_data) {


    /* libopencm3 dma_* routines are too slow.  Use direct register access.

       See Reference manual, 13.3.3 DMA Channels: Channel
       configuration procedure for initialization sequence.  Order is
       important - refer to numbers in comments. */

    /* Disable channel and reset config bits. */
    DMA_CCR(c.d.dma, c.d.chan) = 0;

    /* Reset interrupt flags. */
    DMA_IFCR(c.d.dma) |= DMA_IFCR_CIF(c.d.chan);

    /* 1-2-3 peripheral address, memory address, transfer size */
    DMA_CPAR(c.d.dma, c.d.chan) = (uint32_t)(&SPI_DR(c.spi));
    DMA_CMAR(c.d.dma, c.d.chan) = (uint32_t) data;
    DMA_CNDTR(c.d.dma, c.d.chan) = nb_data;

    /* 4 Priority */
    DMA_CCR(c.d.dma, c.d.chan) = DMA_CCR_PL_VERY_HIGH;

    /* 5 DMA configuration register */
    DMA_CCR(c.d.dma, c.d.chan) |=
          DMA_CCR_MINC                  // memory increment mode
        | (c.bits == 16 ?
           (DMA_CCR_PSIZE_16BIT |       // peripheral access
            DMA_CCR_MSIZE_16BIT) :      // memory access
           (DMA_CCR_PSIZE_8BIT |
            DMA_CCR_MSIZE_8BIT))
        | DMA_CCR_TCIE                  // enable transfer complete interrupt
        | (c.tx ? DMA_CCR_DIR : 0);     // direction

    /* 6 Enable */
    DMA_CCR(c.d.dma, c.d.chan) |= DMA_CCR_EN;

    /* Enable SPI TX/RX */
    SPI_CR2(c.spi) = c.tx ? SPI_CR2_TXDMAEN : SPI_CR2_RXDMAEN;
}

/* Transfer the next buffer.  Can be used e.g. from
   dma1_channel3_isr() to send the next packet.  This needs to be
   preceeded once by hw_spi_start() which does a full configuration,
   and can be used for all subsequent packets. */
INLINE void hw_spi_next(struct hw_spi c, const void *data, uint32_t nb_data) {

    DMA_CMAR(c.d.dma, c.d.chan) = (uint32_t) data;
    DMA_CNDTR(c.d.dma, c.d.chan) = nb_data;

    /* 5 Enable transfer complete interrupt. */
    DMA_CCR(c.d.dma, c.d.chan) |= DMA_CCR_TCIE;

    /* 6 Enable */
    DMA_CCR(c.d.dma, c.d.chan) |= DMA_CCR_EN;

    /* Enable SPI TX/RX */
    SPI_CR2(c.spi) = c.tx ? SPI_CR2_TXDMAEN : SPI_CR2_RXDMAEN;
}

INLINE int hw_spi_ready(struct hw_spi c) {
    return (DMA_ISR(c.d.dma) & DMA_ISR_TCIF(c.d.chan)) != 0;
}
INLINE void hw_spi_ack(struct hw_spi c) {
    if (hw_spi_ready(c)) {
        DMA_IFCR(c.d.dma) |= DMA_IFCR_CTCIF(c.d.chan);  // transfer complete clear
    }
    DMA_CCR(c.d.dma, c.d.chan) &= ~(DMA_CCR_TCIE | DMA_CCR_EN); // disable channel and interrupt
    SPI_CR2(c.spi) &= ~(SPI_CR2_TXDMAEN | SPI_CR2_RXDMAEN); // disable spi dma
}

/* DMA interrupt only indicates data has been transferred to SPI
   peripheral.  To make sure it went out on the wire, monitor
   status bits.  See RM, 25.3.8 Disabling the SPI for proper
   sequence. */
INLINE uint32_t hw_spi_tx_empty(struct hw_spi c) {
    return SPI_SR(c.spi) & SPI_SR_TXE;
}
INLINE uint32_t hw_spi_busy(struct hw_spi c) {
    return SPI_SR(c.spi) & SPI_SR_BSY;
}





/* ---------------------------------------------------------------------
   hw_usart1_slave_* : USART1 receive mode 3Mbaud
   - init:   one time initialization, call before start
   - recv:   call from usart_isr()
*/
INLINE void hw_usart_enable(uint32_t usart) {
    USART_CR1(usart) |= USART_CR1_UE;
}
INLINE void hw_usart_disable(uint32_t usart) {
    USART_CR1(usart) &= ~USART_CR1_UE;
}
INLINE void hw_usart_rx_enable(uint32_t usart) {
    USART_CR1(usart) |= USART_CR1_RE;
}
INLINE void hw_usart_rx_disable(uint32_t usart) {
    USART_CR1(usart) &= ~USART_CR1_RE;
}
INLINE void hw_usart_set_databits(uint32_t usart, uint32_t bits) {
	if (bits == 8) {
		USART_CR1(usart) &= ~USART_CR1_M; /* 8 data bits */
	} else {
		USART_CR1(usart) |= USART_CR1_M;  /* 9 data bits */
	}
}
INLINE void hw_usart_set_stopbits(uint32_t usart, uint32_t stopbits) {
	uint32_t reg32;
	reg32 = USART_CR2(usart);
	reg32 = (reg32 & ~USART_CR2_STOPBITS_MASK) | stopbits;
	USART_CR2(usart) = reg32;
}
INLINE void hw_usart_set_mode(uint32_t usart, uint32_t mode) {
	uint32_t reg32;
	reg32 = USART_CR1(usart);
	reg32 = (reg32 & ~USART_MODE_MASK) | mode;
	USART_CR1(usart) = reg32;
}
INLINE void hw_usart_set_parity(uint32_t usart, uint32_t parity) {
	uint32_t reg32;
	reg32 = USART_CR1(usart);
	reg32 = (reg32 & ~USART_PARITY_MASK) | parity;
	USART_CR1(usart) = reg32;
}
INLINE void hw_usart_set_flow_control(uint32_t usart, uint32_t flowcontrol) {
	uint32_t reg32;
	reg32 = USART_CR3(usart);
	reg32 = (reg32 & ~USART_FLOWCONTROL_MASK) | flowcontrol;
	USART_CR3(usart) = reg32;
}
INLINE void hw_usart_set_baudrate_div(uint32_t usart, uint32_t div) {
	USART_BRR(usart) = div;
}
INLINE void hw_usart_enable_rx_interrupt(uint32_t usart) {
	USART_CR1(usart) |= USART_CR1_RXNEIE;
}
INLINE void hw_usart_disable_rx_interrupt(uint32_t usart) {
	USART_CR1(usart) &= ~USART_CR1_RXNEIE;
}
INLINE void hw_usart_enable_send_ready_interrupt(uint32_t usart) {
	USART_CR1(usart) |= USART_CR1_TXEIE;
}
INLINE void hw_usart_disable_send_ready_interrupt(uint32_t usart) {
	USART_CR1(usart) &= ~USART_CR1_TXEIE;
}
INLINE void hw_usart_enable_send_done_interrupt(uint32_t usart) {
	USART_CR1(usart) |= USART_CR1_TCIE;
}
INLINE void hw_usart_disable_send_done_interrupt(uint32_t usart) {
	USART_CR1(usart) &= ~USART_CR1_TCIE;
}
/* Can be used e.g. to turn of transmitter for RS485. */
INLINE int hw_usart_send_done_interrupt_enabled(uint32_t usart) {
        return !!(USART_CR1(usart) & USART_CR1_TCIE);
}
INLINE int hw_usart_send_ready_interrupt_enabled(uint32_t usart) {
        return !!(USART_CR1(usart) & USART_CR1_TXEIE);
}





/* Note on baud divisors:
   USART1 wrt. APB2 @72Mhz
   USART3 wrt. APB1 @36Mhz
*/



INLINE void hw_usart1_config(uint32_t div, int interrupt) {

    hw_usart_disable(USART1);
    hw_usart_set_databits(USART1, 8);
    hw_usart_set_stopbits(USART1, USART_STOPBITS_1);
    hw_usart_set_mode(USART1, USART_MODE_TX_RX);
    hw_usart_set_parity(USART1, USART_PARITY_NONE);
    hw_usart_set_flow_control(USART1, USART_FLOWCONTROL_NONE);

    // usart_set_baudrate(USART1, 115200);
    // UART1 is clocked with PCLK2 @72MHz
    // lower than BRR=16 doesn't work
    USART_BRR(USART1) = div;

    if (interrupt&1) hw_usart_enable_rx_interrupt(USART1);
    else             hw_usart_disable_rx_interrupt(USART1);
    hw_usart_enable(USART1);
}

INLINE void hw_usart_config(uint32_t usart, uint32_t div, int interrupt) {

    hw_usart_disable(usart);
    hw_usart_set_databits(usart, 8);
    hw_usart_set_stopbits(usart, USART_STOPBITS_1);
    hw_usart_set_mode(usart, USART_MODE_TX_RX);
    hw_usart_set_parity(usart, USART_PARITY_NONE);
    hw_usart_set_flow_control(usart, USART_FLOWCONTROL_NONE);

    // usart_set_baudrate(USART1, 115200);
    // UART1 is clocked with PCLK2 @72MHz
    // lower than BRR=16 doesn't work
    USART_BRR(usart) = div;

    if (interrupt&1) hw_usart_enable_rx_interrupt(usart);
    else             hw_usart_disable_rx_interrupt(usart);
    hw_usart_enable(usart);
}


// Do this before being able to use TIM1_CH2 in PA9
INLINE void hw_usart1_off(void) {
    hw_usart_disable(USART1);
    rcc_periph_clock_disable(RCC_USART1);
}

// It is assumed all chips run at the canonical 72MHz master clock.
// The baud rate divisor is a 16 bit register and devices the master
// clock.
INLINE void hw_usart1_3M(int ie) {
    hw_usart1_config(24, ie); // 3MBaud, interrupt enable configurable
}
INLINE void hw_usart1_115k2(void) {
    hw_usart1_config(625, 0); // 115200 Baud, interrupt disable.
}
INLINE void hw_usart1_250k(void) {
    hw_usart1_config(288, 0); // 250000 Baud, interrupt disable.
}
INLINE void hw_usart1_init(void) {
    rcc_periph_clock_enable(RCC_GPIOA);
    rcc_periph_clock_enable(RCC_USART1);

    //nvic_set_priority(NVIC_USART1_IRQ, 1);
    nvic_enable_irq(NVIC_USART1_IRQ);

    hw_gpio_config(GPIOA, 9, HW_GPIO_CONFIG_ALTFN); // USART1_TX
    hw_gpio_config(GPIOA, 10, HW_GPIO_CONFIG_INPUT); // USART1_RX

    //hw_usart1_3M();
    hw_usart1_115k2();
}
INLINE void hw_usart3_init(void) {
    rcc_periph_clock_enable(RCC_GPIOB);
    rcc_periph_clock_enable(RCC_USART3);

    //nvic_set_priority(NVIC_USART1_IRQ, 1);
    nvic_enable_irq(NVIC_USART3_IRQ);

    hw_gpio_config(GPIOB, 10, HW_GPIO_CONFIG_ALTFN); // USART3_TX
    hw_gpio_config(GPIOB, 11, HW_GPIO_CONFIG_INPUT); // USART3_RX

    hw_usart_config(USART3, 625, 0); // 115200 Baud, interrupt disable.
}

// FIXME: only use getchar (error checking)
INLINE uint8_t hw_usart_recv(uint32_t usart) {
    // SR flags RXNE and ORE cause interrupt
    USART_SR(usart);               // clears ORE if followed by DR read
    uint32_t dr = USART_DR(usart); // clears RXNE
    return dr & USART_DR_MASK;
}
INLINE int hw_usart_getchar(uint32_t usart) {
    // SR flags RXNE and ORE cause interrupt
    uint32_t sr = USART_SR(usart); // clears ORE if followed by DR read
    uint32_t dr = USART_DR(usart); // clears RXNE
    if (sr & 0xF) return -1; // check errors: [ORE NE FE PE]
    return dr & USART_DR_MASK;
}
/* Same as getchar, but in case of error create a negative error code
 * containing SR and DR. */
INLINE int hw_usart_getchar_nsr(uint32_t usart) {
    // SR flags RXNE and ORE cause interrupt
    int32_t  sr = USART_SR(usart);   // clears ORE if followed by DR read
    uint32_t dr = USART_DR(usart);   // clears RXNE
    if (sr & 0xF) return (((uint32_t)(-1)) << 12) | ((sr & 0xF) << 8) | (dr & 0xFF);
    return dr & USART_DR_MASK;
}
INLINE int hw_usart_recv_ready(uint32_t usart) {
    return USART_SR(usart) & USART_SR_RXNE;
}
INLINE int hw_usart_send_ready(uint32_t usart) {
    return USART_SR(usart) & USART_SR_TXE;
}
//INLINE void hw_usart_send_ready_ack(uint32_t usart) {
//    USART_SR(usart) &= ~USART_SR_TXE;
//}
INLINE void hw_usart_send(uint32_t usart, uint8_t cmd) {
    // usart_send(USART1, cmd);
    USART_DR(usart) = (cmd & USART_DR_MASK); // inline
}
INLINE int hw_usart_send_done(uint32_t usart) {
    return USART_SR(usart) & USART_SR_TC;
}
INLINE void hw_usart_send_flush(uint32_t usart) {
    // Busywait until all data went out on the wire.
    while(!hw_usart_send_ready(usart));
    while(!hw_usart_send_done(usart));
}
/* I'm seeing some odd behavoir calling send_ready and send_done in
   sequence.  This does the same in a single read. */
INLINE int hw_usart_flushed(uint32_t usart) {
    uint32_t mask = USART_SR_TC | USART_SR_TXE;
    uint32_t bits = USART_SR(usart) & mask;
    return bits == mask;
}

/* Old routines, hardcoded to usart1 */
INLINE uint8_t hw_usart1_recv(void)        { return hw_usart_recv(USART1); }
INLINE int hw_usart1_getchar(void)         { return hw_usart_getchar(USART1); }
INLINE int hw_usart1_getchar_nsr(void)     { return hw_usart_getchar_nsr(USART1); }
INLINE int hw_usart1_recv_ready(void)      { return hw_usart_recv_ready(USART1); }
INLINE int hw_usart1_send_ready(void)      { return hw_usart_send_ready(USART1); }
INLINE int hw_usart1_send_done(void)       { return hw_usart_send_done(USART1); }
//INLINE void hw_usart1_send_ready_ack(void) { hw_usart_send_ready_ack(USART1); }
INLINE void hw_usart1_send(uint8_t cmd)    { hw_usart_send(USART1, cmd); }
INLINE void hw_usart1_send_flush(void)     { hw_usart_send_flush(USART1); }



/* ---------------------------------------------------------------------
   PA0-PC0 -> EXTI0
   PA1-PC1 -> EXTI1
   ...
   hw_exti_* : EXTIx external interrupt
   - init:   one time initialization, call before start
   - arm:    call from anywhere, enables trigger
   - ack:    call from ISR
*/
struct hw_exti {
    uint32_t rcc_gpio;
    uint32_t irq;
    uint32_t pin;
    uint32_t gpio;
    uint32_t trigger;
};

// Note that 5-9 and 10-15 use shared interrupts.
// Grep libopencm3/include/libopencm3/stm32/f1/nvic.h for exti0_isr
//                      rcc_gpio   irq                 pin  gpio   trigger
#define HW_EXTI_A0_B  { RCC_GPIOA, NVIC_EXTI0_IRQ,      0,  GPIOA, EXTI_TRIGGER_BOTH }    // exti0_isr()
#define HW_EXTI_B15_B { RCC_GPIOB, NVIC_EXTI15_10_IRQ, 15,  GPIOB, EXTI_TRIGGER_BOTH }    // exti15_10_isr()
#define HW_EXTI_A3_F  { RCC_GPIOA, NVIC_EXTI3_IRQ,      3,  GPIOA, EXTI_TRIGGER_FALLING } // exti3_isr()
#define HW_EXTI_A3_F  { RCC_GPIOA, NVIC_EXTI3_IRQ,      3,  GPIOA, EXTI_TRIGGER_FALLING } // exti3_isr()
#define HW_EXTI_A10_F { RCC_GPIOA, NVIC_EXTI15_10_IRQ, 10,  GPIOA, EXTI_TRIGGER_FALLING } // exti15_10_isr()
#define HW_EXTI_B11_F { RCC_GPIOB, NVIC_EXTI15_10_IRQ, 11,  GPIOB, EXTI_TRIGGER_FALLING } // exti15_10_isr()



// libopencm3-examples/examples/stm32/f1/stm32-h103/exti_rising_falling/exti_rising_falling.c
INLINE void hw_exti_init(struct hw_exti c) {
    rcc_periph_clock_enable(c.rcc_gpio);
    rcc_periph_clock_enable(RCC_AFIO);
    //nvic_set_priority(c.irq, 1);
    nvic_enable_irq(c.irq);
}
INLINE void hw_exti_enable_request(struct hw_exti c) {
    uint32_t exti = 1 << c.pin;
    EXTI_IMR |= exti; // Interrupt Mask Register
    EXTI_EMR |= exti; // Event Mask Register
}
INLINE void hw_exti_disable_request(struct hw_exti c) {
    uint32_t exti = 1 << c.pin;
    EXTI_IMR &= ~exti;  // Interrupt Mask Register
    EXTI_EMR &= ~exti;  // Event Mask Register
}
INLINE void hw_exti_trigger_swi(struct hw_exti c) {
    uint32_t exti = 1 << c.pin;
    EXTI_SWIER = exti;  // Trigger the event
}

INLINE void hw_exti_arm(struct hw_exti c) {
    uint32_t exti = 1 << c.pin;
    exti_select_source(exti, c.gpio);
    exti_set_trigger(exti, c.trigger);
    // hw_gpio_config(c.gpio, c.pin, HW_GPIO_CONFIG_INPUT); // FIXME: do this manually
    hw_exti_enable_request(c);
}
INLINE void hw_exti_ack(struct hw_exti c) {
    /// Inline the functions below.  Use bitbanding for rmw, normal write otherwise.
    // exti_disable_request(EXTI6); // don't trigger again
    // exti_reset_request(EXTI6); // ack current pending bit
    //*hw_bitband(&EXTI_IMR, c.pin) = 0; // rmw

    /* Prevent future triggering by disabling event in mask register. */
    *hw_bitband(&EXTI_EMR, c.pin) = 0; // rmw

    /* Set pending register to zero = acknowledge. */
    EXTI_PR = 1 << c.pin; // w
}

INLINE void hw_exti_clear_pending(struct hw_exti c) {
    /* 10.3.6 Pending register (EXTI_PR)
       Bit is cleared by writing a '1' into the bit. */
    EXTI_PR = 1 << c.pin; // w
}



/* ---------------------------------------------------------------------
   hw_swi_* : EXTIx software events
   - init:    one time initialization
   - trigger: raise interrupt flag
   - ack:     call from ISR

   Note that pin edge events in hw_exti_* can also be triggered
   manually, but for pure software interrupts that do not have an
   external pin associated, we provide a separate construct.

*/
struct hw_swi {
    uint32_t irq;
    uint32_t pin;
};

//                  irq                 pin
#define HW_SWI_0  { NVIC_EXTI0_IRQ,      0 }  // ext0_isr()
#define HW_SWI_1  { NVIC_EXTI1_IRQ,      1 }  // ext1_isr()
#define HW_SWI_15 { NVIC_EXTI15_10_IRQ, 15 }  // ext15_10_isr()

// Note that 5-9 and 10-15 use shared interrupts.

INLINE void hw_swi_init(struct hw_swi c) {
    nvic_enable_irq(c.irq);
    uint32_t exti = 1 << c.pin;
    EXTI_IMR |= exti;
    EXTI_EMR |= exti;
    EXTI_PR = 1 << c.pin; // wouldn't work without
}
INLINE void hw_swi_ack(struct hw_swi c) {
    /* FIXME: This was in the hw_exti_ code when I copied it to
       hw_swi_, but I can't explain it.  Why is this EMR write
       necessary?  Doesn't that mask setting disable interrupts? */
    *hw_bitband(&EXTI_EMR, c.pin) = 0; // rmw
    EXTI_PR = 1 << c.pin; // w
}
INLINE void hw_swi_trigger(struct hw_swi c) {
    EXTI_SWIER = 1 << c.pin;
    //*hw_bitband(&EXTI_SWIER, c.pin) = 1; // rmw
}




/* DEBUG */
INLINE void hw_PA_init(uint32_t pin) {
    rcc_periph_clock_enable(RCC_GPIOA);
    hw_gpio_config(GPIOA, pin, HW_GPIO_CONFIG_OUTPUT);
}
/* Bit banding doesn't seem to make a lot of sense here since a
   rmw operation isn't necessary. */
INLINE void hw_PA_on(uint32_t pin)    { hw_gpio_high(GPIOA, pin); }
INLINE void hw_PA_off(uint32_t pin)   { hw_gpio_low(GPIOA, pin); }
INLINE void hw_PA_pulse(uint32_t pin) {
    hw_PA_on(pin);
    hw_PA_off(pin);
}

INLINE void hw_PB_init(uint32_t pin) {
    rcc_periph_clock_enable(RCC_GPIOB);
    hw_gpio_config(GPIOB, pin, HW_GPIO_CONFIG_OUTPUT);
}
INLINE void hw_PB_on(uint32_t pin)    { hw_gpio_high(GPIOB, pin); }
INLINE void hw_PB_off(uint32_t pin)   { hw_gpio_low(GPIOB, pin); }
INLINE void hw_PB_pulse(uint32_t pin) {
    hw_PB_on(pin);
    hw_PB_off(pin);
}

/* Bootloader HAL.  The emulator only uses io and stubs out the _init
 * and _poll methods. */
void hw_bootloader_usb_init(void);
void hw_bootloader_usb_poll(void);

#endif // HW_STM32F103_H


