#ifndef IS31FL3738_H
#define IS31FL3738_H

/*
http://www.issi.com/WW/pdf/IS31FL3738.pdf

https://github.com/znuh/microbit-corona-scanner/blob/5d326d059c5150f3e3a640ecf7d43ef0d4023cbe/source/is31fl3738.cpp
https://github.com/znuh/microbit-corona-scanner/blob/f15cae02b6863d3bdbab36cbbc235ba3d9d3575a/source/is31fl3738.h

git clone https://github.com/znuh/microbit-corona-scanner.git
  source/is31fl3738.cpp
  source/is31fl3738.h

101 0000
    0101
    1010
    1111

Determined by ADDR pin.

*/



#include <stdint.h>


#ifndef IS31FL13738_ADDR
#define IS31FL13738_ADDR 0x50
#endif


struct is31fl3738 {
    /* Code has been simplified to optimize for happy path.  Errors
       are passed in context struct instead of as return value. Return
       value is valid only when s->status == 0. That makes code a lot
       easier to read, and creates more compact binaries. */
    int status;
};

/* Registers are paged.
   Original init code sets some config then switches to PWM page. */

#define IS31FL3738_REG_CMD       0xFD
#define IS31FL3738_REG_WRLOCK    0xFE
#define IS31FL3738_REG_INTMASK   0xF0
#define IS31FL3738_REG_INTSTATUS 0xF1

/* IS31FL3738_REG_CMD selects one of these command register pages. */
#define IS31FL3738_CMD_LEDCTL    0x00  /* 00-17 on/off 18-2D open 30-45 short */
#define IS31FL3738_CMD_PWM       0x01  /* 00-BF PWM */
#define IS31FL3738_CMD_ABM       0x02  /* 00-BF Auto Breath Mode (ABM) */
#define IS31FL3738_CMD_FUNC	 0x03  /* 00 configuration register, 11 reset register */


#define IS31FL3738_FUNC_CONFIG   0x00
#define IS31FL3738_FUNC_GCCR     0x01
#define IS31FL3738_FUNC_ABM1R1   0x02
#define IS31FL3738_FUNC_ABM1R2   0x03
#define IS31FL3738_FUNC_ABM1R3   0x04
#define IS31FL3738_FUNC_ABM1R4   0x05
#define IS31FL3738_FUNC_ABM2R1   0x06
#define IS31FL3738_FUNC_ABM2R2   0x07
#define IS31FL3738_FUNC_ABM2R3   0x08
#define IS31FL3738_FUNC_ABM2R4   0x09
#define IS31FL3738_FUNC_ABM3R1   0x0A
#define IS31FL3738_FUNC_ABM3R2   0x0B
#define IS31FL3738_FUNC_ABM3R3   0x0C
#define IS31FL3738_FUNC_ABM3R4   0x0D
#define IS31FL3738_FUNC_TIMEUPD  0x0E
#define IS31FL3738_FUNC_PULLUP   0x0F
#define IS31FL3738_FUNC_PULLDOWN 0x10
#define IS31FL3738_FUNC_RESET    0x11

#define IS31FL3738_WRLOCK_MAGIC  0xC5



/* Write takes first byte to be register address, subsequent data with auto-increment. */
static inline void is31fl3738_write(struct is31fl3738 *s, uint8_t reg, const uint8_t *buf, uint32_t len) {
    uint8_t header[1] = { reg };
    s->status =
        IS31FL3738_HAL_I2C_TRANSIMIT(
            IS31FL3738_I2C_ADDR,
            header, sizeof(header),
            buf, len);
    IS31FL3738_HAL_I2C_STOP();
}
#define IS31FL3738_WRITE(s, addr, ...) {                        \
        const uint8_t _data[] = {__VA_ARGS__};                  \
        is31fl3738_write(s, addr, _data, sizeof(_data));        \
}
static inline void is31fl3738_read(struct is31fl3738 *s, uint8_t reg, uint8_t *buf, uint32_t len) {
    /* The device uses write followed by read.  No repeated start. */
    is31fl3738_write(s, reg, 0, 0);
    if (s->status) goto stop;
    s->status =
        IS31FL3738_HAL_I2C_RECEIVE(
            IS31FL3738_I2C_ADDR,
            buf, len);
  stop:
    IS31FL3738_HAL_I2C_STOP();
}

/* Reading uses restart condition. */


static inline void is31fl3738_write_byte(struct is31fl3738 *s, uint8_t reg, uint8_t byte) {
    is31fl3738_write(s, reg, &byte, 1);
}
static inline void is31fl3738_page(struct is31fl3738 *s, uint8_t page) {
    /* To change the page, the command register needs to be unlocked first. */
    is31fl3738_write_byte(s, IS31FL3738_REG_WRLOCK, IS31FL3738_WRLOCK_MAGIC);
    if (s->status) return;
    /* Then the page can be selected. */
    is31fl3738_write_byte(s, IS31FL3738_REG_CMD, page);
}

static void is31fl3738_init(struct is31fl3738 *s) {
    is31fl3738_page(s, IS31FL3738_CMD_LEDCTL);
    if(s->status) goto error;
        // Each LED has 4 bits on and off state
#if 0 // original code
    IS31FL3738_WRITE(
        s, 0x00,
        0xff, 0x03, 0xff, 0x03,   /* SW1 */
        0xff, 0x03, 0xff, 0x03,   /* SW2 */
        0xff, 0x03, 0xff, 0x03,   /* SW3 */
        0xff, 0x03, 0xff, 0x03,   /* SW4 */
        0xff, 0x03, 0xff, 0x03    /* SW5 */
        );
    IS31FL3738_WRITE(
        s, 0x00,
        0x01, 0xFF);              /* normal operation, set GCC to 0xFF */
    is31fl3738_page(s, IS31FL3738_CMD_FUNC);
    if(s->status) goto error;
    IS31FL3738_WRITE(
        s, 0x00,
        0x01, 0x80);              /* normal operation, set GCC to 0xFF */
    if(s->status) goto error;
#else // uvc
    IS31FL3738_WRITE(
        s, 0x00,
        0xff, 0xff, 0xff, 0xff,   /* SW1 */
        0xff, 0xff, 0xff, 0xff,   /* SW2 */
        0xff, 0xff, 0xff, 0xff,   /* SW3 */
        0x00, 0x00, 0x00, 0x00,   /* SW4 */
        0x00, 0x00, 0x00, 0x00    /* SW5 */
        );
    is31fl3738_page(s, IS31FL3738_CMD_FUNC);
    if(s->status) goto error;
    IS31FL3738_WRITE(
        s, 0x00,
        0x01, 0xFF);              /* normal operation, set GCC to 0xFF */
    if(s->status) goto error;
#endif
    is31fl3738_page(
        s, IS31FL3738_CMD_PWM);   /* switch to PWM page access */
    if(s->status) goto error;
  error:
    return;
}

#endif


