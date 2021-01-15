#ifndef HW_I2C_H
#define HW_I2C_H

/* Just I2C1 on STM32F103 on default pins:
   'B7 SDA
   'B6 SCL */


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
struct hw_i2c_transmit_state {
    uint32_t tries; // retry counter
    uint32_t sr; // status register or ad-hoc status code
    uint32_t i; // loop counter
    uint8_t slave; // slave address

    const uint8_t *hdr;  uint32_t hdr_len;
    const uint8_t *data; uint32_t data_len;
};
struct hw_i2c_receive_state {
    uint32_t tries; // retry counter
    uint32_t sr; // status register or ad-hoc status code
    uint32_t i; // loop counter
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


static inline uint32_t hw_i2c_transmit_(struct hw_i2c_transmit_state *s, struct hw_i2c c) {
#define S s
#include "hw_i2c_transmit_body.h"
#undef S
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
        .tries = HW_I2C_TRIES,
        .slave = slave,
        .hdr   = hdr,  .hdr_len  = hdr_len,
        .data  = data, .data_len = data_len,
    };
    return hw_i2c_transmit_(&s, c);
}



static inline uint32_t hw_i2c_receive_(struct hw_i2c_receive_state *s, struct hw_i2c c) {
#define S s
#include "hw_i2c_receive_body.h"
#undef S
}

static inline uint32_t hw_i2c_receive(
    struct hw_i2c c, uint32_t slave,
    uint8_t *data, uint32_t data_len) {

    /* Note that we have to convert from const to non-const here to be
       able to reuse the same state struct for read and write, which
       is very convenient.  Access is read-only for write, so this is
       ok. */
    struct hw_i2c_receive_state s = {
        .tries = HW_I2C_TRIES,
        .slave = slave,
        .data  = data, .data_len = data_len,
    };
    return hw_i2c_receive_(&s, c);
}

static inline void hw_i2c_stop(struct hw_i2c c) {
    I2C_CR1(c.i2c) |= I2C_CR1_STOP;
}




#endif
