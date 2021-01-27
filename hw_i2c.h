#ifndef HW_I2C_H
#define HW_I2C_H

/* Just I2C1 on STM32F103 on default pins:
   'B7 SDA
   'B6 SCL */

/* Code is structured as an sm.h protothread.
   Blocking wrappers are provided. */
#include "sm.h"

// FIXME: make this a parameter

#include "log.h"
#include "hw_stm32f103.h"

/* Note that there are two versions of the i2c header. The file for
   STM32F1 is libopencm3/stm32/f1/i2c.h which includes
   libopencm3/stm32/common/i2c_common_v1.h

   This seems to be related to the specific chips.  Following the
   includes I find:

   i2c v1: f1, f2, f4, l1
   i2c v2: f0, f3, f7, g0, l0, l4

*/

#include <libopencm3/stm32/i2c.h>

/* For debugging, I'm going to inline a couple of these functions so
   the register interaction is more clear.  These are all copied from
   libopencm3/lib/stm32/common/i2c_common_v1.c and prefixed with hw_
*/
static inline void hw_i2c_reset(uint32_t i2c) {
    switch (i2c) {
    case I2C1: hw_rcc_periph_reset_pulse(RST_I2C1); break;
    case I2C2: hw_rcc_periph_reset_pulse(RST_I2C2); break;
    }
}
static inline void hw_i2c_peripheral_disable(uint32_t i2c)        { I2C_CR1(i2c) &= ~I2C_CR1_PE; }
static inline void hw_i2c_peripheral_enable(uint32_t i2c)         { I2C_CR1(i2c) |=  I2C_CR1_PE; }
static inline void hw_i2c_set_fast_mode(uint32_t i2c)             { I2C_CCR(i2c) |=  I2C_CCR_FS; }
static inline void hw_i2c_set_standard_mode(uint32_t i2c)         { I2C_CCR(i2c) &= ~I2C_CCR_FS; }
static inline void hw_i2c_set_trise(uint32_t i2c, uint16_t trise) { I2C_TRISE(i2c) = trise;      }
static inline void hw_i2c_set_clock_frequency(uint32_t i2c, uint8_t freq) {
    uint16_t reg16;
    reg16 = I2C_CR2(i2c) & 0xffc0; /* Clear bits [5:0]. */
    reg16 |= freq;
    I2C_CR2(i2c) = reg16;
}
static inline void hw_i2c_set_ccr(uint32_t i2c, uint16_t freq) {
    uint16_t reg16;
    reg16 = I2C_CCR(i2c) & 0xf000; /* Clear bits [11:0]. */
    reg16 |= freq;
    I2C_CCR(i2c) = reg16;
}
static inline void hw_i2c_set_speed(uint32_t i2c, enum i2c_speeds speed, uint32_t clock_megahz) {
    hw_i2c_set_clock_frequency(i2c, clock_megahz);
    switch(speed) {
    case i2c_speed_fm_400k:
        hw_i2c_set_fast_mode(i2c);
        hw_i2c_set_ccr(i2c, clock_megahz * 5 / 6);
        hw_i2c_set_trise(i2c, clock_megahz + 1);
        break;
    default:
        /* fall back to standard mode */
    case i2c_speed_sm_100k:
        hw_i2c_set_standard_mode(i2c);
        /* x Mhz / (100kHz * 2) */
        hw_i2c_set_ccr(i2c, clock_megahz * 5);
        /* Sm mode, (100kHz) freqMhz + 1 */
        hw_i2c_set_trise(i2c, clock_megahz + 1);
        break;
    }
}


/* This is code from i2c_common_v1 inlined.  There was a bug not
   checking AF, so I no longer trust it was tested well.
   I've added a simple timeout mechanism in every busy loop.
*/
#define HW_I2C_TRIES 100000  // FIXME: Map this to absolute time


struct hw_i2c {
    uint32_t rcc;
    uint32_t i2c;
    uint32_t gpio;
    uint32_t scl;
    uint32_t sda;
    uint32_t speed;
};

// FIXME: Name the error codes.  For now they are ad-hoc.

int hw_i2c_sda(struct hw_i2c c) { return hw_gpio_read(c.gpio, c.sda); }
int hw_i2c_scl(struct hw_i2c c) { return hw_gpio_read(c.gpio, c.scl); }

static inline void hw_i2c_unblock(struct hw_i2c c) {
    // FIXME: doesn't seem to work. revisit.  I'm still getting errors
    // where the bus is being pulled down and I don't know who.

    /* Unblock bus devices by toggling clock at startup.
       https://www.i2c-bus.org/i2c-primer/analysing-obscure-problems/blocked-bus/
    */
    uint32_t nb_clocks = 16;
    uint32_t half_period_us = 10;
    hw_gpio_high(c.gpio, c.scl); // pull direction
    hw_gpio_high(c.gpio, c.sda); // pull direction
    for (int i=0; i<nb_clocks; i++) {
        hw_busywait_us(half_period_us);
        hw_gpio_low(c.gpio, c.scl);
        hw_busywait_us(half_period_us);
        hw_gpio_high(c.gpio, c.scl);
    }
}

static inline void hw_i2c_setup_swjenable(struct hw_i2c c, uint32_t swjenable) {

    rcc_periph_clock_enable(c.rcc);
    hw_i2c_reset(c.i2c);

    /* Note that swjenable is necesary here, because that register is
       write-only, so we cannot rely on read,modify,write. */
    gpio_primary_remap(swjenable, AFIO_MAPR_I2C1_REMAP);

    /* pin debug */
    //hw_gpio_config(c.gpio, c.sda, HW_GPIO_CONFIG_OUTPUT);
    //hw_gpio_low(c.gpio, c.sda); // pull direction

    // FIXME: make this configurable to use external resistors?
    // Might not be necessary to disable pull if internal pull is weak.


    hw_gpio_high(c.gpio, c.scl); // pull direction
    hw_gpio_high(c.gpio, c.sda); // pull direction
    hw_gpio_config(c.gpio, c.scl, HW_GPIO_CONFIG_ALTFN_OPEN_DRAIN);
    hw_gpio_config(c.gpio, c.sda, HW_GPIO_CONFIG_ALTFN_OPEN_DRAIN);


    hw_i2c_peripheral_disable(c.i2c);
    //i2c_enable_interrupt(c.i2c, I2C_CR2_ITEVTEN);
    hw_i2c_set_speed(c.i2c, c.speed, 72);
    hw_i2c_peripheral_enable(c.i2c);
}

static inline void hw_i2c_setup(struct hw_i2c c) {
    /* FIXME: THIS RESETS JTAG/SWD PINS! */
    uint32_t swjenable = 0;
    hw_i2c_setup_swjenable(c, swjenable);
}




/* Note that many protocols need a header, so we allow for that. */

struct hw_i2c_control_state {
    uint32_t tries; // retry counter
    uint32_t sr; // status register or ad-hoc status code
    uint32_t i; // loop counter
};

struct hw_i2c_transmit_state {
    struct hw_i2c_control_state ctrl;
    void *next; // sm.h resume point
    uint8_t slave; // slave address
    const uint8_t *hdr;  uint32_t hdr_len;
    const uint8_t *data; uint32_t data_len;
};
struct hw_i2c_receive_state {
    struct hw_i2c_control_state ctrl;
    void *next; // sm.h resume point
    uint8_t slave; // slave address
    uint8_t *hdr;  uint32_t hdr_len;
    uint8_t *data; uint32_t data_len;
};
#if 0
static inline uint32_t hw_i2c_timeout(struct hw_i2c_state *s) {
    if (!s->tries--) {
        s->sr = 0x30000;
    }
    else {
        s->sr = 0;
    }
}
#endif



static inline void hw_i2c_stop(struct hw_i2c c) {
    I2C_CR1(c.i2c) |= I2C_CR1_STOP;
}


#define HW_I2C_ERROR(s, status_code) {                          \
        s->ctrl.sr = status_code;                               \
        goto error;                                             \
    }



#if 0
/* Old, blocking operation.  Keep this here for a while, make sure we
   can easily revert to the old implementation. */
#define HW_I2C_WHILE(s, condition, status_code)                 \
    while (condition) {                                         \
        if (!s->ctrl.tries--) { HW_I2c_ERROR(s, status_code); } \
    }
                                  }

#else
/* New, sm.h based tick method. */
#define HW_I2C_WHILE(s, condition, status_code)                 \
    if (!SM_WAIT_COUNT(s, !(condition), s->ctrl.tries)) {       \
        HW_I2C_ERROR(s, status_code);                           \
    }
#endif

// TRANSMIT, NON BLOCKING

static inline void hw_i2c_transmit_init(
    struct hw_i2c c,
    struct hw_i2c_transmit_state *s,
    uint32_t slave,
    const uint8_t *hdr, uint32_t hdr_len,
    const uint8_t *data, uint32_t data_len) {

    memset(s,0,sizeof(*s));
    s->ctrl.tries = HW_I2C_TRIES;
    s->slave = slave;
    s->hdr = hdr;
    s->hdr_len = hdr_len;
    s->data = data;
    s->data_len = data_len;
}

static inline uint32_t hw_i2c_transmit_tick(struct hw_i2c c, struct hw_i2c_transmit_state *s) {

    SM_RESUME(s);

    //if (I2C_SR2(c.i2c) & I2C_SR2_BUSY) HW_I2C_ERROR(s, 0x30000);

    HW_I2C_WHILE(s, (I2C_SR2(c.i2c) & I2C_SR2_BUSY), 0x30000);

    /* send start */
    I2C_CR1(c.i2c) |= I2C_CR1_START;

    /* Wait for master mode selected */
    HW_I2C_WHILE(s, !((I2C_SR1(c.i2c) & I2C_SR1_SB) &
                      (I2C_SR2(c.i2c) & (I2C_SR2_MSL | I2C_SR2_BUSY))), 0x30001);

    /* Send 7bit address */
    I2C_DR(c.i2c) = (uint8_t)((s->slave << 1) | I2C_WRITE);

    /* Wait for address bit done or error. */
    HW_I2C_WHILE(s, !(s->ctrl.sr = I2C_SR1(c.i2c)), 0x30002);
    if (!(s->ctrl.sr & I2C_SR1_ADDR)) goto error;

    /* Clearing ADDR condition sequence. */
    (void)I2C_SR2(c.i2c);

    if (s->hdr) {
        for (s->ctrl.i = 0; s->ctrl.i <s->hdr_len; s->ctrl.i++) {
            I2C_DR(c.i2c) = s->hdr[s->ctrl.i];
            HW_I2C_WHILE(s, !(s->ctrl.sr = I2C_SR1(c.i2c)), 0x30003);
            if (!(s->ctrl.sr | I2C_SR1_BTF)) goto error;
        }
    }
    if (s->data) {
        for (s->ctrl.i = 0; s->ctrl.i < s->data_len; s->ctrl.i++) {
            I2C_DR(c.i2c) = s->data[s->ctrl.i];
            HW_I2C_WHILE(s, !(s->ctrl.sr = I2C_SR1(c.i2c)), 0x30004);
            if (!(s->ctrl.sr | I2C_SR1_BTF)) goto error;
        }
    }
    s->ctrl.sr = 0;
    return 0;

  error:
    LOG("i2c transmit error: %x\n", s->ctrl.sr);
    return s->ctrl.sr;
}



// RECEIVE, NON BLOCKING

static inline void hw_i2c_receive_init(
    struct hw_i2c c,
    struct hw_i2c_receive_state *s,
    uint32_t slave,
    uint8_t *data, uint32_t data_len) {

    memset(s,0,sizeof(*s));
    s->ctrl.tries = HW_I2C_TRIES;
    s->slave = slave;
    s->data = data;
    s->data_len = data_len;
}

static inline uint32_t hw_i2c_receive_tick(struct hw_i2c c, struct hw_i2c_receive_state *s) {
    SM_RESUME(s);

    //HW_I2C_WHILE ((I2C_SR2(c.i2c) & I2C_SR2_BUSY));


    /* Send start */
    I2C_CR1(c.i2c) |= I2C_CR1_START;

    /* Wait for master mode selected */
    HW_I2C_WHILE(s, !((I2C_SR1(c.i2c) & I2C_SR1_SB) &
                      (I2C_SR2(c.i2c) & (I2C_SR2_MSL | I2C_SR2_BUSY))), 0x30011);

    /* Send 7bit address */
    I2C_DR(c.i2c) = (uint8_t)((s->slave << 1) | I2C_READ);

    /* Enable ack */
    I2C_CR1(c.i2c) |= I2C_CR1_ACK;

    /* Wait for address bit done or error. */
    HW_I2C_WHILE(s, !(s->ctrl.sr = I2C_SR1(c.i2c)), 0x30012);

    if (!(s->ctrl.sr & I2C_SR1_ADDR)) {
        infof("i2c receive error waiting for addr: SR1=%x\n", s->ctrl.sr);
        return s->ctrl.sr;
    }

    /* Clearing ADDR condition sequence. */
    (void)I2C_SR2(c.i2c);

    for (s->ctrl.i = 0; s->ctrl.i < s->data_len; ++(s->ctrl.i)) {
        if (s->ctrl.i == s->data_len-1) {
            /* Disable ack */
            I2C_CR1(c.i2c) &= ~I2C_CR1_ACK;
        }

        HW_I2C_WHILE(s, !(s->ctrl.sr = I2C_SR1(c.i2c)), 0x30013);
        if (!(I2C_SR1(c.i2c) & I2C_SR1_RxNE)) {
            infof("i2c receive data byte %d: SR1=%x\n", s->ctrl.i, s->ctrl.sr);
        }
        s->data[s->ctrl.i] = I2C_DR(c.i2c) & 0xff;
    }
    s->ctrl.sr = 0;
    return 0;
error:
    infof("i2c receive error: SR1=%x\n", s->ctrl.sr);
    return s->ctrl.sr;
}



// BLOCKING WRAPPERS

static inline uint32_t hw_i2c_transmit(
    struct hw_i2c c, uint32_t slave,
    const uint8_t *hdr, uint32_t hdr_len,
    const uint8_t *data, uint32_t data_len) {

    struct hw_i2c_transmit_state s;
    hw_i2c_transmit_init(c, &s, slave, hdr, hdr_len, data, data_len);
    while (SM_WAITING == hw_i2c_transmit_tick(c, &s));
    return s.ctrl.sr;
}

static inline uint32_t hw_i2c_receive(
    struct hw_i2c c, uint32_t slave,
    uint8_t *data, uint32_t data_len) {

    struct hw_i2c_receive_state s;
    hw_i2c_receive_init(c, &s,  slave, data, data_len);
    while (SM_WAITING == hw_i2c_receive_tick(c, &s));
    return s.ctrl.sr;
}





#endif
