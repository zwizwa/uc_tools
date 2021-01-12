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

static inline uint32_t hw_i2c_transmit(
    struct hw_i2c c, uint32_t slave,
    const uint8_t *hdr, uint32_t hdr_len,
    const uint8_t *data, uint32_t data_len) {

    uint32_t tries = HW_I2C_TRIES;

    uint32_t sr;

    HW_I2C_WHILE ((I2C_SR2(c.i2c) & I2C_SR2_BUSY)) {
        if (!tries--) {
            sr = 0x30000;
            goto error;
        }
    }

    /* send start */
    I2C_CR1(c.i2c) |= I2C_CR1_START;

    /* Wait for master mode selected */
    HW_I2C_WHILE (!((I2C_SR1(c.i2c) & I2C_SR1_SB) &
             (I2C_SR2(c.i2c) & (I2C_SR2_MSL | I2C_SR2_BUSY)))) {
        if (!tries--) {
            sr = 0x30001;
            goto error;
        }
    }

    /* Send 7bit address */
    I2C_DR(c.i2c) = (uint8_t)((slave << 1) | I2C_WRITE);

    /* Wait for address bit done or error. */
    HW_I2C_WHILE(!(sr = I2C_SR1(c.i2c))) {
        if (!tries--) {
            sr = 0x30002;
            goto error;
        }
    }
    if (!(sr & I2C_SR1_ADDR)) goto error;

    /* Clearing ADDR condition sequence. */
    (void)I2C_SR2(c.i2c);

    if (hdr) {
        for (size_t i = 0; i < hdr_len; i++) {
            I2C_DR(c.i2c) = hdr[i];
            HW_I2C_WHILE (!(sr = I2C_SR1(c.i2c))) {
                if (!tries--) {
                    sr = 0x30003;
                    goto error;
                }
            }
            if (!(sr | I2C_SR1_BTF)) goto error;
        }
    }
    if (data) {
        for (size_t i = 0; i < data_len; i++) {
            I2C_DR(c.i2c) = data[i];
            HW_I2C_WHILE (!(sr = I2C_SR1(c.i2c))) {
                if (!tries--) {
                    sr = 0x30004;
                    goto error;
                }
            }
            if (!(sr | I2C_SR1_BTF)) goto error;
        }
    }
    return 0;

  error:
    LOG("i2c transmit error: %x\n", sr);
    return sr;
}
static inline uint32_t hw_i2c_receive(
    struct hw_i2c c, uint32_t slave,
    uint8_t *data, uint32_t len) {

    uint32_t tries = HW_I2C_TRIES;


    uint32_t sr;

    //HW_I2C_WHILE ((I2C_SR2(c.i2c) & I2C_SR2_BUSY));


    /* Send start */
    I2C_CR1(c.i2c) |= I2C_CR1_START;

    /* Wait for master mode selected */
    HW_I2C_WHILE (!((I2C_SR1(c.i2c) & I2C_SR1_SB) &
             (I2C_SR2(c.i2c) & (I2C_SR2_MSL | I2C_SR2_BUSY)))) {
        if (!tries--) {
            sr = 0x30011;
            goto error;
        }
    }

    /* Send 7bit address */
    I2C_DR(c.i2c) = (uint8_t)((slave << 1) | I2C_READ);

    /* Enable ack */
    I2C_CR1(c.i2c) |= I2C_CR1_ACK;

    /* Wait for address bit done or error. */
    HW_I2C_WHILE(!(sr = I2C_SR1(c.i2c))) {
        if (!tries--) {
            sr = 0x30012;
            goto error;
        }
    }
    if (!(sr & I2C_SR1_ADDR)) {
        infof("i2c receive error waiting for addr: SR1=%x\n", sr);
        return sr;
    }

    /* Clearing ADDR condition sequence. */
    (void)I2C_SR2(c.i2c);

    for (size_t i = 0; i < len; ++i) {
        if (i == len-1) {
            /* Disable ack */
            I2C_CR1(c.i2c) &= ~I2C_CR1_ACK;
        }

        HW_I2C_WHILE (!(sr = I2C_SR1(c.i2c))) {
            if (!tries--) {
                sr = 0x30013;
                goto error;
            }
        }
        if (!(I2C_SR1(c.i2c) & I2C_SR1_RxNE)) {
            infof("i2c receive data byte %d: SR1=%x\n", i, sr);
        }
        data[i] = I2C_DR(c.i2c) & 0xff;
    }

    return 0;
error:
    infof("i2c receive error: SR1=%x\n", sr);
    return sr;

}

static inline void hw_i2c_stop(struct hw_i2c c) {
    I2C_CR1(c.i2c) |= I2C_CR1_STOP;
}




#endif
