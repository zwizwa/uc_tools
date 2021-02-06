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

#include "slice.h"

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
static inline void hw_rcc_periph_clock_enable(enum rcc_periph_clken clken) {
    _RCC_REG(clken) |= _RCC_BIT(clken);
}
static inline void hw_i2c_reset(uint32_t i2c) {
    switch (i2c) {
    case I2C1: hw_rcc_periph_reset_pulse(RST_I2C1); break;
    case I2C2: hw_rcc_periph_reset_pulse(RST_I2C2); break;
    }
}
static inline void hw_i2c_set_swrst(uint32_t i2c, int val) {
    if (val) I2C_CR1(i2c) |=  I2C_CR1_SWRST;
    else     I2C_CR1(i2c) &= ~I2C_CR1_SWRST;
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
    uint32_t rst;
    uint32_t i2c;
    uint32_t gpio;
    uint32_t scl;
    uint32_t sda;
    uint32_t speed;
};

// FIXME: Name the error codes.  For now they are ad-hoc.

int hw_i2c_read_sda(struct hw_i2c c) { return hw_gpio_read(c.gpio, c.sda); }
int hw_i2c_read_scl(struct hw_i2c c) { return hw_gpio_read(c.gpio, c.scl); }

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

/* To allow checking the bus during peripheral reset. */
static inline void hw_i2c_reset_check(struct hw_i2c c, void (*check)(void)) {
    hw_gpio_config(c.gpio, c.scl, HW_GPIO_CONFIG_INPUT);
    hw_gpio_config(c.gpio, c.sda, HW_GPIO_CONFIG_INPUT);
    check();
    hw_rcc_periph_off(c.rst);
    hw_busywait_us(10);
    check();
    hw_rcc_periph_on(c.rst);
    hw_busywait_us(10);
}

static inline void hw_i2c_setup_swjenable(struct hw_i2c c, uint32_t swjenable) {

    hw_gpio_high(c.gpio, c.scl); // pull direction
    hw_gpio_high(c.gpio, c.sda); // pull direction
    hw_gpio_config(c.gpio, c.scl, HW_GPIO_CONFIG_INPUT);
    hw_gpio_config(c.gpio, c.sda, HW_GPIO_CONFIG_INPUT);

    hw_rcc_periph_clock_enable(c.rcc);
#if 0
    hw_i2c_reset(c.i2c);
#else
    /* FIXME: I don't know what I'm doing... */
    hw_i2c_set_swrst(c.i2c, 1);
    hw_busywait_us(10);
    hw_i2c_set_swrst(c.i2c, 0);
#endif

    /* Note that swjenable is necesary here, because that register is
       write-only, so we cannot rely on read,modify,write. */
    gpio_primary_remap(swjenable, AFIO_MAPR_I2C1_REMAP);

    /* pin debug */
    //hw_gpio_config(c.gpio, c.sda, HW_GPIO_CONFIG_OUTPUT);
    //hw_gpio_low(c.gpio, c.sda); // pull direction

    // FIXME: make this configurable to use external resistors?
    // Might not be necessary to disable pull if internal pull is weak.




    hw_i2c_peripheral_disable(c.i2c);
    //i2c_enable_interrupt(c.i2c, I2C_CR2_ITEVTEN);
    hw_i2c_set_speed(c.i2c, c.speed, 72);
    hw_i2c_peripheral_enable(c.i2c);

    hw_gpio_config(c.gpio, c.scl, HW_GPIO_CONFIG_ALTFN_OPEN_DRAIN);
    hw_gpio_config(c.gpio, c.sda, HW_GPIO_CONFIG_ALTFN_OPEN_DRAIN);

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
    const_slice_uint8_t hdr;
    const_slice_uint8_t data;
    uint8_t slave; // slave address
};
struct hw_i2c_receive_state {
    struct hw_i2c_control_state ctrl;
    void *next; // sm.h resume point
    slice_uint8_t data;
    uint8_t slave; // slave address
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
    s->hdr.buf = hdr;
    s->hdr.len = hdr_len;
    s->data.buf = data;
    s->data.len = data_len;
}


#define HW_I2C_TAG_MASK        0xFFFF0000
#define HW_I2C_TAG_SR1_UNKNOWN 0x00000000
#define HW_I2C_TAG_HANDLED     0x00010000

/*
Error codes:
0003xxxx timeouts, see code for location
0001xxxx handled errors
0000xxxx SR1

Common codes:
00030000 bus busy timout at start of transmit
00000400 AF acknowledge failure
*/

/* Note that all macros that cross suspension points could also go
   into sub-machines.  For now, keep them as macros.  They are simpler
   to refactor at this point due to the presence of 'error' label. */

/* Error condition handling can be split into a function.  We want to
   indicate two things upstream: was there an error, and did it get
   handled such that i2c is in a good state again to do a new
   transaction. */
void hw_i2c_handle_sr1(const struct hw_i2c c, struct hw_i2c_control_state *ctrl) {
    if (ctrl->sr & I2C_SR1_AF) {
        /* Acknowledge failure. */
        I2C_SR1(c.i2c) &= ~I2C_SR1_AF;
        ctrl->sr |= HW_I2C_TAG_HANDLED;
        hw_i2c_stop(c);
    }
}
/* I'm having trouble doing soft reset after transaction error.  This
   handles all conditions that need to be cleared in software. */
void hw_i2c_soft_reset(const struct hw_i2c c) {
    if (I2C_SR2(c.i2c) & I2C_SR2_BUSY) {
        hw_i2c_stop(c);
    }
    I2C_SR1(c.i2c) = 0;
}

#define HW_I2C_TRANSMIT_START(_c, _s, _addr, _error_base) {             \
        HW_I2C_WHILE(_s, (I2C_SR2(_c.i2c) & I2C_SR2_BUSY), _error_base + 0x00); \
        /* send start */                                                \
        I2C_CR1(_c.i2c) |= I2C_CR1_START;                               \
        /* Wait for master mode selected */                             \
        HW_I2C_WHILE(_s, !((I2C_SR1(_c.i2c) & I2C_SR1_SB) &             \
                           (I2C_SR2(_c.i2c) & (I2C_SR2_MSL | I2C_SR2_BUSY))), _error_base + 0x01); \
        /* Send 7bit address */                                         \
        I2C_DR(_c.i2c) = (uint8_t)((_addr << 1) | I2C_WRITE);           \
        /* Wait for address bit done or error. */                       \
        HW_I2C_WHILE(_s, !(_s->ctrl.sr = I2C_SR1(_c.i2c)), _error_base + 0x02); \
        if (!(_s->ctrl.sr & I2C_SR1_ADDR)) {                            \
            hw_i2c_handle_sr1(_c, &_s->ctrl);                           \
            goto error;                                                 \
        }                                                               \
        /* Clearing ADDR condition sequence. */                         \
        (void)I2C_SR2(_c.i2c);                                          \
    }

#define HW_I2C_TRANSMIT_BYTE(_c, _s, _byte, _error_code) {              \
        I2C_DR(_c.i2c) = _byte;                                         \
        HW_I2C_WHILE(_s, !(_s->ctrl.sr = I2C_SR1(_c.i2c)), _error_code); \
        if (!(_s->ctrl.sr | I2C_SR1_BTF)) goto error;                   \
    }

/* Slice can be empty. */
#define HW_I2C_TRANSMIT_SLICE(_c, _s, _slice, _error_code)              \
        if (_slice.buf) {                                               \
            for (_s->ctrl.i = 0; _s->ctrl.i < _slice.len; _s->ctrl.i++) { \
                HW_I2C_TRANSMIT_BYTE(_c, _s, _slice.buf[s->ctrl.i], _error_code); \
            }                                                           \
        }

#define HW_I2C_RECEIVE_START(_c, _s, _addr, _error_base) {              \
        /* Send start */                                                \
        I2C_CR1(_c.i2c) |= I2C_CR1_START;                               \
        /* Wait for master mode selected */                             \
        HW_I2C_WHILE(_s, !((I2C_SR1(_c.i2c) & I2C_SR1_SB) &             \
                           (I2C_SR2(_c.i2c) & (I2C_SR2_MSL | I2C_SR2_BUSY))), 0x30011); \
        /* Send 7bit address */                                         \
        I2C_DR(_c.i2c) = (uint8_t)((_addr << 1) | I2C_READ);            \
        /* Enable ack */                                                \
        I2C_CR1(_c.i2c) |= I2C_CR1_ACK;                                 \
        /* Wait for address bit done or error. */                       \
        HW_I2C_WHILE(_s, !(_s->ctrl.sr = I2C_SR1(_c.i2c)), 0x30012);    \
        if (!(_s->ctrl.sr & I2C_SR1_ADDR)) {                            \
            infof("i2c receive error waiting for addr: SR1=%x\n", _s->ctrl.sr); \
            return _s->ctrl.sr;                                         \
        }                                                               \
        /* Clearing ADDR condition sequence. */                         \
        (void)I2C_SR2(_c.i2c);                                          \
    }

#define HW_I2C_RECEIVE_SLICE(_c, _s, _slice, _error_code)               \
    for (_s->ctrl.i = 0; _s->ctrl.i < _s->data.len; ++(_s->ctrl.i)) {   \
        if (_s->ctrl.i == _s->data.len-1) {                             \
            /* Disable ack */                                           \
            I2C_CR1(_c.i2c) &= ~I2C_CR1_ACK;                            \
        }                                                               \
        HW_I2C_WHILE(_s, !(_s->ctrl.sr = I2C_SR1(_c.i2c)), 0x30013);    \
        if (!(I2C_SR1(_c.i2c) & I2C_SR1_RxNE)) {                        \
            infof("i2c receive data byte %d: SR1=%x\n", _s->ctrl.i, _s->ctrl.sr); \
        }                                                               \
        _s->data.buf[_s->ctrl.i] = I2C_DR(_c.i2c) & 0xff;               \
    }


static inline uint32_t hw_i2c_transmit_tick(struct hw_i2c c, struct hw_i2c_transmit_state *s) {
    SM_RESUME(s);
    HW_I2C_TRANSMIT_START(c, s, s->slave, 0x30000);
    HW_I2C_TRANSMIT_SLICE(c, s, s->hdr,   0x30003);
    HW_I2C_TRANSMIT_SLICE(c, s, s->data,  0x30004);
    s->ctrl.sr = 0;
    SM_HALT(s);
  error:
    SM_HALT_STATUS(s, s->ctrl.sr);
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
    s->data.buf = data;
    s->data.len = data_len;
}

static inline uint32_t hw_i2c_receive_tick(struct hw_i2c c, struct hw_i2c_receive_state *s) {
    SM_RESUME(s);
    HW_I2C_RECEIVE_START(c, s, s->slave, 0x30000);
    HW_I2C_RECEIVE_SLICE(c, s, s->data,  0x30013);
    s->ctrl.sr = 0;
    SM_HALT(s);
  error:
    SM_HALT_STATUS(s, s->ctrl.sr);
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
