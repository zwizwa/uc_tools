#ifndef HW_I2C_H
#define HW_I2C_H

/* Just I2C1 on STM32F103 on default pins:
   'B7 SDA
   'B6 SCL */



#include "hw_stm32f103.h"
#include <libopencm3/stm32/i2c.h>

struct hw_i2c {
    uint32_t rcc;
    uint32_t i2c;
    uint32_t gpio;
    uint32_t scl;
    uint32_t sda;
    uint32_t speed;
};


static inline void hw_i2c_setup(struct hw_i2c c) {

    rcc_periph_clock_enable(c.rcc);
    i2c_reset(c.i2c);

    gpio_primary_remap(0, AFIO_MAPR_I2C1_REMAP);



    /* pin debug */
    //hw_gpio_config(c.gpio, c.sda, HW_GPIO_CONFIG_OUTPUT);
    //hw_gpio_low(c.gpio, c.sda); // pull direction


    hw_gpio_high(c.gpio, c.scl); // pull direction
    hw_gpio_high(c.gpio, c.sda); // pull direction
    hw_gpio_config(c.gpio, c.scl, HW_GPIO_CONFIG_ALTFN_OPEN_DRAIN);
    hw_gpio_config(c.gpio, c.sda, HW_GPIO_CONFIG_ALTFN_OPEN_DRAIN);


    i2c_peripheral_disable(c.i2c);
    //i2c_enable_interrupt(c.i2c, I2C_CR2_ITEVTEN);
    i2c_set_speed(c.i2c, c.speed, 72);
    i2c_peripheral_enable(c.i2c);
}



/* This is code from i2c_common_v1 inlined.  There was a bug not
   checking AF, so I no longer trust it was tested well.

   FIXME: REMOVE ALL BUSY LOOPS.  Replace with finite loops and a
   reset to recover bad conditions. */

/* Note that many protocols need a header, so we allow for that. */

static inline uint32_t hw_i2c_transmit(
    struct hw_i2c c, uint32_t slave,
    const uint8_t *hdr, uint32_t hdr_len,
    const uint8_t *data, uint32_t data_len) {

    uint32_t sr;

    while ((I2C_SR2(c.i2c) & I2C_SR2_BUSY));

    /* send start */
    I2C_CR1(c.i2c) |= I2C_CR1_START;

    /* Wait for master mode selected */
    while (!((I2C_SR1(c.i2c) & I2C_SR1_SB) &
             (I2C_SR2(c.i2c) & (I2C_SR2_MSL | I2C_SR2_BUSY))));

    /* Send 7bit address */
    I2C_DR(c.i2c) = (uint8_t)((slave << 1) | I2C_WRITE);

    /* Wait for address bit done or error. */
    while(!(sr = I2C_SR1(c.i2c)));
    if (!(sr & I2C_SR1_ADDR)) goto error;

    /* Clearing ADDR condition sequence. */
    (void)I2C_SR2(c.i2c);

    if (hdr) {
        for (size_t i = 0; i < hdr_len; i++) {
            I2C_DR(c.i2c) = hdr[i];
            while (!(sr = I2C_SR1(c.i2c)));
            if (!(sr | I2C_SR1_BTF)) goto error;
        }
    }
    if (data) {
        for (size_t i = 0; i < data_len; i++) {
            I2C_DR(c.i2c) = data[i];
            while (!(sr = I2C_SR1(c.i2c)));
            if (!(sr | I2C_SR1_BTF)) goto error;
        }
    }
    return 0;

  error:
    infof("i2c transmit error: SR1=%x\n", sr);
    return sr;
}
static inline uint32_t hw_i2c_receive(
    struct hw_i2c c, uint32_t slave,
    uint8_t *data, uint32_t len) {

    uint32_t sr;

    //while ((I2C_SR2(c.i2c) & I2C_SR2_BUSY));


    /* Send start */
    I2C_CR1(c.i2c) |= I2C_CR1_START;

    /* Wait for master mode selected */
    while (!((I2C_SR1(c.i2c) & I2C_SR1_SB) &
             (I2C_SR2(c.i2c) & (I2C_SR2_MSL | I2C_SR2_BUSY))));

    /* Send 7bit address */
    I2C_DR(c.i2c) = (uint8_t)((slave << 1) | I2C_READ);

    /* Enable ack */
    I2C_CR1(c.i2c) |= I2C_CR1_ACK;

    /* Wait for address bit done or error. */
    while(!(sr = I2C_SR1(c.i2c)));
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

        while (!(sr = I2C_SR1(c.i2c)));
        if (!(I2C_SR1(c.i2c) & I2C_SR1_RxNE)) {
            infof("i2c receive data byte %d: SR1=%x\n", i, sr);
        }
        data[i] = I2C_DR(c.i2c) & 0xff;
    }

    return 0;

}

static inline void hw_i2c_stop(struct hw_i2c c) {
    I2C_CR1(c.i2c) |= I2C_CR1_STOP;
}




#endif
