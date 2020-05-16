#ifndef PWM_BITSTREAM_H
#define PWM_BITSTREAM_H

/* 1. BITBANG */

/* Require this to be overridden. */
#ifndef PWM_BITSTREAM_SET_PIN
// #warn PWM_BITSTREAM_SET_PIN not defined
#define PWM_BITSTREAM_SET_PIN(s, val)
#endif


/* State machine for implementing a 100/110 PWM bitstream protocol, as
   used in Staapl one-wire protocol, biased Sigma/Delta DAC, WS2812
   LED strip, ...   Note that for WS2812 the STM32F103 is too slow. */
struct pwm_bitstream;
typedef void (*pwm_bitstream_set_pin)(struct pwm_bitstream *, int val);
typedef void (*pwm_bitstream_done)(struct pwm_bitstream *);



struct pwm_bitstream {
    uint8_t *buf;       // pixel buffer
    uint32_t len_bits;  // pixel buffer length in bits
    uint32_t read_bits; // read pointer, indexed in bits
    int32_t phase;      // negative is preroll pulses
    pwm_bitstream_done done;
    uint32_t next_bit;
};

/* PWM_BITSTREAM bit-bang state machine. */
static inline void pwm_bitstream_init(
    struct pwm_bitstream *s,
    pwm_bitstream_done done) {
    s->buf = 0;
    s->len_bits = 0;
    s->read_bits = 0;
    s->phase = 0;
    s->done = done;
}

/* Note that caller will need to ensure that _update will be called
   after this, e.g. by enabling an interrupt. */
static inline void pwm_bitstream_start(
    struct pwm_bitstream *s, uint8_t *buf, uint32_t len) {
    s->buf = buf;
    s->len_bits = len * 8;
    s->read_bits = 0;
    s->phase = 0;
    s->next_bit = 0;
}


/* PWM_BITSTREAM update function should be called once every 450 uS.  See
   spec doc. */
static inline void pwm_bitstream_update(
    struct pwm_bitstream *s) {

    /* This is pipelined to ensure constant delay from isr start to
     * setting the pin. */
    PWM_BITSTREAM_SET_PIN(s, s->next_bit);

    /* Compute the next bit or stop condition. */

    uint32_t r = s->read_bits; // single read to allow struct to be volatile
    if (r >= s->len_bits) {
        s->done(s);
        return;
    }
    int p = s->phase;
    switch(p) {
    default:
        /* Preroll */
        s->phase = p + 1;
        s->next_bit = 0;
        return;
    case 0:
        s->phase = 1;
        s->next_bit = 1;
        return;
    case 1: {
        s->next_bit = 1 & (s->buf[r >> 3] >> ((7-r) & 7));
        s->phase = 2;
        return;
    }
    case 2:
        s->next_bit = 0;
        s->phase = 0;
        s->read_bits = r + 1;  // single write
        return;
    }
}

/* 2. BYTE STREAM GENERATION */

/* Alternative implementation, generating a byte stream that can be
   sent out, e.g. using DMA with a 400ns clock signal.  This assumes
   the buffers are large enough to not have to perform bounds checking
   in the converter loop. */

// FIXME: This compiles but is not tested.

#include "bitbuf.h"
void pwm_bitstream_write(uint8_t *dst_buf, uint8_t *src_buf, uint32_t nb_bits) {
    struct bitbuf src, dst;
    bitbuf_init(&src, src_buf);
    bitbuf_init(&dst, dst_buf);
    for(uint32_t i=0; i<nb_bits; i++) {
        uint32_t bit = bitbuf_read(&src);
        bitbuf_write(&dst, 1);
        bitbuf_write(&dst, bit);
        bitbuf_write(&dst, 0);
    }
    bitbuf_flush(&dst);
}

#endif
