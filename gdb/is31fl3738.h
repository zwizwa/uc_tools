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
#define IS31FL3738_CMD_LEDCTL    0x00
#define IS31FL3738_CMD_PWM       0x01
#define IS31FL3738_CMD_ABM       0x02
#define IS31FL3738_CMD_FUNC	 0x03


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
static inline void is31fl3738_read(struct is31fl3738 *s, uint8_t reg, uint8_t *buf, uint32_t len) {
    /* The device uses write followed by read.  No repeated start. */
    is31fl3738_write(s, reg, 0, 0);
    if (s->status) return;
    s->status =
        IS31FL3738_HAL_I2C_RECEIVE(
            IS31FL3738_I2C_ADDR,
            buf, len);
    IS31FL3738_HAL_I2C_STOP();
}

/* Reading uses restart condition. */


static inline void is31fl3738_write_byte(struct is31fl3738 *s, uint8_t reg, uint8_t byte) {
    is31fl3738_write(s, reg, &byte, 1);
}
static inline void is31fl3738_cmd(struct is31fl3738 *s, uint8_t cmd, const uint8_t *data, uint32_t data_len) {
    is31fl3738_write_byte(s, IS31FL3738_REG_WRLOCK, IS31FL3738_WRLOCK_MAGIC);
    if (s->status) return;
    is31fl3738_write_byte(s, IS31FL3738_REG_CMD, cmd);
    if (data_len) {
        // FIXME: if there is data, data[0] is always 00.  Read the datasheet and explain.
        is31fl3738_write(s, data[0], data+1, data_len-1);
    }
}

// FIXME: check the original init routine.  Not quite clear.

/* Recoded */
#define IS31FL3738_CMD(s, cmd, ...) {                 \
        const uint8_t _data[] = {__VA_ARGS__};        \
        is31fl3738_cmd(s, cmd, _data, sizeof(_data)); \
}

static inline void is31fl3738_init(struct is31fl3738 *s) {
    IS31FL3738_CMD(
        s, IS31FL3738_CMD_LEDCTL,
        0x00,
        0xff, 0x03, 0xff, 0x03,   /* SW1 */
        0xff, 0x03, 0xff, 0x03,   /* SW2 */
        0xff, 0x03, 0xff, 0x03,   /* SW3 */
        0xff, 0x03, 0xff, 0x03,   /* SW4 */
        0xff, 0x03, 0xff, 0x03);  /* SW5 */
    IS31FL3738_CMD(
        s, IS31FL3738_CMD_FUNC,
        0x00, 0x01, 0x80);        /* normal operation, set GCC to 128 */
    IS31FL3738_CMD(
        s, IS31FL3738_CMD_PWM);   /* switch to PWM page access */
}


#if 0

/* Ok this encodes message length in a weird way.
   Let's not do it like that.
   Command has high bit set, low 7 are command.
   Next byte is length.
   Rest is payload.
*/


// Look at this again... is it sending a data tail?
static inline void is31fl3738_init(struct is31fl3738 *s) {
    char tmp;
    for(const uint8_t *init = init_data; *init; init++) {
        uint8_t v = *init;
        if(v&0x80) {
            is31fl3738_cmd(s, v&0x7f);
        else {
            res = I2C_WRITE(SLAVE_ADDR, (const char *)init+1, v);
            init+=v;
        }
    }
}

#endif

#endif


