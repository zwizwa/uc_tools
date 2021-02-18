#ifndef MOD_I2C_TRACK
#define MOD_I2C_TRACK

/* This needs to respond immediately so is run from interrupt.  We use
   the EXTI mechanism, and implement the state machine in sm_cond.h
   conditional style, which means it will always always evaluate the
   state of the pins and will not rely on the information provided by
   the interrupt mechanism.  I.e. the state machine is essentially
   just polling mode, and the events are used to poll on _possible_
   state change.
*/

#include "sm.h"

/* The intention here is to create trace logs that interleave properly
   with the test driver. */
#ifndef LOG_I2C
#define LOG_I2C(...)
#endif

// 3.0 Abstract machine

#define I2C_TRACK_SCL (1<<1)
#define I2C_TRACK_SDA (1<<0)

#define I2C_WAIT_C1D1_IDLE(s)  SM_WAIT(s,   s->bus == 0b11)
#define I2C_WAIT_C1D0_START(s) SM_WAIT(s,   s->bus == 0b10)
#define I2C_WAIT_C1Dx_HI(s)    SM_WAIT(s,   s->bus &  0b10)
#define I2C_WAIT_C0Dx_LO(s)    SM_WAIT(s, !(s->bus &  0b10))

struct i2c_track {
    void *next;
    uint8_t bus;    // current bus value
    uint8_t bus0;   // previous bus value (for STOP detection)
    uint8_t bit;    // burrent bit
    uint8_t sreg;   // shift register
};
struct i2c_track i2c_track;
void i2c_track_init(struct i2c_track *s) {
    memset(s,0,sizeof(*s));
    s->bus = 0b11;
}
// CD -> CD
// 11    10  start
// 10    11  stop
// 1_    0_  write edge
// 0_    1_  read edge

// Machine, in the abstract:
// wait start
// wait read edge, read x 8
// wait write edge, write ack
//
// so machine can just wait on a condition (DC combo), we don't
// need to track history, that is implicit in state machine.

// FIXME: This still needs some work.
// 1. Convert the inner machine to a byte read/write machine similar to bitbang.
// 2. Focus on getting test_i2c_bitbang.c to work properly first before moving this to uc.

uint32_t i2c_track_tick(struct i2c_track *s) {
    SM_RESUME(s);

    I2C_WAIT_C1D1_IDLE(s);

  stop:
    for(;;) {
        I2C_WAIT_C1D0_START(s);
        I2C_WAIT_C0Dx_LO(s); // write edge
        LOG_I2C(" start");

        for(;;) {

            // data byte
            for(s->bit = 0; s->bit < 9; s->bit++) {
                I2C_WAIT_C1Dx_HI(s); // read edge
                // sda must be stable until write edge
                s->sreg = (s->bus & 1) | (s->sreg << 1);

                LOG_I2C(" b%d=%d", s->bit, s->bus & 1);

                // To distinguish between normal clock transition and
                // STOP, we wait for a change and inspect it.
                s->bus0 = s->bus;
                SM_WAIT(s, s->bus != s->bus0);

                // SDA 0->1 while SCL=1 is a STOP condition.  It
                // always indicates end of transmission, even in the
                // middle of a byte.
                if ((s->bus0 == 0b10) &&
                    (s->bus  == 0b11)) {
                    LOG_I2C(" stop");
                    goto stop;
                }
                // SCL 1->0 with SDA=dontcare indicates a normal clock
                // edge, indicating a new bit is coming and the loop
                // can continue.


            }
        }
    }
}

#ifndef EMU

// 3.1 TIM
// See mod_3level.c for inspiration
// Note that I2C slave is synchronouse, so timer is likely not necessary.

#if 0
#define TICKS_PER_US 100

const struct hw_delay hw_tim[] = {
//          rcc       irq            tim   psc
//-------------------------------------------------------
    [3] = { RCC_TIM3, NVIC_TIM3_IRQ, TIM3, 72*TICKS_PER_US },
};
#define TIM 3
#define C_TIM hw_tim[TIM]
void HW_TIM_ISR(TIM)(void) {
    hw_delay_ack(C_TIM);
    // uint16 diff = 123;
    //hw_delay_arm(C_TIM, diff);
    //hw_delay_trigger(C_TIM);

}
void i2c_tester_tim_init(void) {
    hw_delay_init(C_TIM, 0xFFFF, 1 /*enable interrupt*/);
    hw_delay_arm(C_TIM, 1);
    hw_delay_trigger(C_TIM);
}
#endif



// 3.2 EXTI

// This extends hw_exti_ack() for multi-event setup.
// FIXME: Comment why both EMR nd PR are written.
static inline void hw_exti_do_ack(uint32_t pin) {
    *hw_bitband(&EXTI_EMR, pin) = 0; // rmw
    EXTI_PR = 1 << pin; // w
}
static uint32_t isr_count;
void exti9_5_isr(void) {
    /* We just need to be woken up on change, so it's ok if this
       handles a simultaneous change on 8 and 9.  */
    hw_exti_do_ack(8);
    hw_exti_do_ack(9);
    isr_count++;
    i2c_track.bus = (i2c_read_scl() << 1) | i2c_read_sda();
    i2c_track_tick(&i2c_track);



}
// This extends hw_exti_arm() for multi-event setup.
static inline void hw_exti_do_arm(uint32_t gpio, uint32_t pin, uint32_t trigger) {
    uint32_t exti = 1 << pin;
    exti_select_source(exti, gpio);
    exti_set_trigger(exti, trigger);
    exti_enable_request(exti);
}
void i2c_tester_exti_init(void) {
    /* The config is special because we handle multiple pins in a
       single interrupt, so do it manually here.  For inspiration, see
       hw_stm32103.h */
    rcc_periph_clock_enable(RCC_GPIOB);
    rcc_periph_clock_enable(RCC_AFIO);
    nvic_enable_irq(NVIC_EXTI9_5_IRQ);
    hw_exti_do_arm(GPIOB, 8, EXTI_TRIGGER_BOTH);
    hw_exti_do_arm(GPIOB, 9, EXTI_TRIGGER_BOTH);
}

#endif



#endif

