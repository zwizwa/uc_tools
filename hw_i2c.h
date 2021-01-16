#ifndef HW_I2C_H
#define HW_I2C_H

/* Just I2C1 on STM32F103 on default pins:
   'B7 SDA
   'B6 SCL */

/* Code is structured as an sm.h protothread.  Blocking wrappers are
 * provided. */


// FIXME: make this a parameter

#include "log.h"
#include "hw_stm32f103.h"
#include <libopencm3/stm32/i2c.h>

/* This is code from i2c_common_v1 inlined.  There was a bug not
   checking AF, so I no longer trust it was tested well.
   I've added a simple timeout mechanism in every busy loop.
*/
#define HW_I2C_TRIES 100000  // FIXME: Map this to absolute time

/* For protothread wrapping. */
#ifndef HW_I2C_WHILE
#define HW_I2C_WHILE while
#endif


struct hw_i2c {
    uint32_t rcc;
    uint32_t i2c;
    uint32_t gpio;
    uint32_t scl;
    uint32_t sda;
    uint32_t speed;
};

// FIXME: Name the error codes.  For now they are ad-hoc.

static inline void hw_i2c_setup(struct hw_i2c c) {

    rcc_periph_clock_enable(c.rcc);
    i2c_reset(c.i2c);

    gpio_primary_remap(0, AFIO_MAPR_I2C1_REMAP);



    /* pin debug */
    //hw_gpio_config(c.gpio, c.sda, HW_GPIO_CONFIG_OUTPUT);
    //hw_gpio_low(c.gpio, c.sda); // pull direction

    // FIXME: make this configurable to use external resistors?
    // Might not be necessary to disable pull if internal pull is weak.
    hw_gpio_high(c.gpio, c.scl); // pull direction
    hw_gpio_high(c.gpio, c.sda); // pull direction
    hw_gpio_config(c.gpio, c.scl, HW_GPIO_CONFIG_ALTFN_OPEN_DRAIN);
    hw_gpio_config(c.gpio, c.sda, HW_GPIO_CONFIG_ALTFN_OPEN_DRAIN);


    i2c_peripheral_disable(c.i2c);
    //i2c_enable_interrupt(c.i2c, I2C_CR2_ITEVTEN);
    i2c_set_speed(c.i2c, c.speed, 72);
    i2c_peripheral_enable(c.i2c);
}

static inline void hw_i2c_unblock(struct hw_i2c c) {
    // FIXME: Not yet implemented.  Send out 16 clocks to unblock any
    // device that might be holding the bus, e.g. due to reset during
    // a transaction.
}



/* Note that many protocols need a header, so we allow for that. */

struct hw_i2c_control_state {
    uint32_t tries; // retry counter
    uint32_t sr; // status register or ad-hoc status code
    uint32_t i; // loop counter
};

struct hw_i2c_transmit_state {
    struct hw_i2c_control_state ctrl;
    uint8_t slave; // slave address
    const uint8_t *hdr;  uint32_t hdr_len;
    const uint8_t *data; uint32_t data_len;
};
struct hw_i2c_receive_state {
    struct hw_i2c_control_state ctrl;
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

#define HW_I2C_WHILE_TRIES(condition, status_code)      \
    HW_I2C_WHILE (condition) {                          \
        if (!s->ctrl.tries--) {                         \
            s->ctrl.sr = status_code;                   \
            goto error;                                 \
        }                                               \
    }

static inline uint32_t hw_i2c_transmit_(struct hw_i2c_transmit_state *s, struct hw_i2c c) {
    HW_I2C_WHILE_TRIES ((I2C_SR2(c.i2c) & I2C_SR2_BUSY), 0x30000);

    /* send start */
    I2C_CR1(c.i2c) |= I2C_CR1_START;

    /* Wait for master mode selected */
    HW_I2C_WHILE_TRIES (!((I2C_SR1(c.i2c) & I2C_SR1_SB) &
                          (I2C_SR2(c.i2c) & (I2C_SR2_MSL | I2C_SR2_BUSY))), 0x30001);

    /* Send 7bit address */
    I2C_DR(c.i2c) = (uint8_t)((s->slave << 1) | I2C_WRITE);

    /* Wait for address bit done or error. */
    HW_I2C_WHILE_TRIES(!(s->ctrl.sr = I2C_SR1(c.i2c)), 0x30002);
    if (!(s->ctrl.sr & I2C_SR1_ADDR)) goto error;

    /* Clearing ADDR condition sequence. */
    (void)I2C_SR2(c.i2c);

    if (s->hdr) {
        for (s->ctrl.i = 0; s->ctrl.i <s->hdr_len; s->ctrl.i++) {
            I2C_DR(c.i2c) = s->hdr[s->ctrl.i];
            HW_I2C_WHILE_TRIES (!(s->ctrl.sr = I2C_SR1(c.i2c)), 0x30003);
            if (!(s->ctrl.sr | I2C_SR1_BTF)) goto error;
        }
    }
    if (s->data) {
        for (s->ctrl.i = 0; s->ctrl.i < s->data_len; s->ctrl.i++) {
            I2C_DR(c.i2c) = s->data[s->ctrl.i];
            HW_I2C_WHILE_TRIES (!(s->ctrl.sr = I2C_SR1(c.i2c)), 0x30004);
            if (!(s->ctrl.sr | I2C_SR1_BTF)) goto error;
        }
    }
    return 0;

  error:
    LOG("i2c transmit error: %x\n", s->ctrl.sr);
    return s->ctrl.sr;
}


static inline uint32_t hw_i2c_transmit(
    struct hw_i2c c, uint32_t slave,
    const uint8_t *hdr, uint32_t hdr_len,
    const uint8_t *data, uint32_t data_len) {

    /* Note that we have to convert from const to non-const here to be
       able to reuse the same state struct for read and write, which
       is very convenient.  Access is read-only for write, so this is
       ok. */
    struct hw_i2c_transmit_state s = {
        .ctrl = {
            .tries = HW_I2C_TRIES,
        },
        .slave = slave,
        .hdr   = hdr,  .hdr_len  = hdr_len,
        .data  = data, .data_len = data_len,
    };
    return hw_i2c_transmit_(&s, c);
}



static inline uint32_t hw_i2c_receive_(struct hw_i2c_receive_state *s, struct hw_i2c c) {
    //HW_I2C_WHILE ((I2C_SR2(c.i2c) & I2C_SR2_BUSY));


    /* Send start */
    I2C_CR1(c.i2c) |= I2C_CR1_START;

    /* Wait for master mode selected */
    HW_I2C_WHILE_TRIES (!((I2C_SR1(c.i2c) & I2C_SR1_SB) &
                          (I2C_SR2(c.i2c) & (I2C_SR2_MSL | I2C_SR2_BUSY))), 0x30011);

    /* Send 7bit address */
    I2C_DR(c.i2c) = (uint8_t)((s->slave << 1) | I2C_READ);

    /* Enable ack */
    I2C_CR1(c.i2c) |= I2C_CR1_ACK;

    /* Wait for address bit done or error. */
    HW_I2C_WHILE_TRIES(!(s->ctrl.sr = I2C_SR1(c.i2c)), 0x30012);

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

        HW_I2C_WHILE_TRIES (!(s->ctrl.sr = I2C_SR1(c.i2c)), 0x30013);
        if (!(I2C_SR1(c.i2c) & I2C_SR1_RxNE)) {
            infof("i2c receive data byte %d: SR1=%x\n", s->ctrl.i, s->ctrl.sr);
        }
        s->data[s->ctrl.i] = I2C_DR(c.i2c) & 0xff;
    }

    return 0;
error:
    infof("i2c receive error: SR1=%x\n", s->ctrl.sr);
    return s->ctrl.sr;
}

static inline uint32_t hw_i2c_receive(
    struct hw_i2c c, uint32_t slave,
    uint8_t *data, uint32_t data_len) {

    /* Note that we have to convert from const to non-const here to be
       able to reuse the same state struct for read and write, which
       is very convenient.  Access is read-only for write, so this is
       ok. */
    struct hw_i2c_receive_state s = {
        .ctrl = {
            .tries = HW_I2C_TRIES,
        },
        .slave = slave,
        .data  = data, .data_len = data_len,
    };
    return hw_i2c_receive_(&s, c);
}

static inline void hw_i2c_stop(struct hw_i2c c) {
    I2C_CR1(c.i2c) |= I2C_CR1_STOP;
}




#endif
