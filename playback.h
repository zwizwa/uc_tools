#ifndef PLAYBACK_H
#define PLAYBACK_H

#include <stdint.h>


/* Quick and dirty sample playback, e.g. to feed PWM.  Uses unsigned
   samples with midpoint at 128.  If buf is not set, it plays back a
   reference sawtooth. */
struct playback {
    uint8_t *buf;
    uint32_t len;
    uint32_t read;  // 24.8 fractional
    uint32_t speed; // 24.8 fractional
};


/* No interpolation. This sounds schirpy at 8kHz.  Returns uint32_t
   but is limited to uint8_t range. */
static inline uint32_t playback_next_8p0(struct playback *p) {
    uint32_t read_i = p->read >> 8;
    uint32_t duty;
    if (read_i < p->len) {
        duty = p->buf[read_i];
        p->read += p->speed;
    }
    else {
        duty = 128; // silence
    }
    return duty;
}

/* Linear interpolation, with result returned as 8.8 fixed point. */
static inline uint32_t playback_next_li_8p8(struct playback *p) {
    uint32_t read_i = p->read >> 8;
    uint32_t duty;
    if (read_i < p->len-1) {
        uint32_t a = p->buf[read_i];
        uint32_t b = p->buf[read_i+1];
        uint32_t frac = p->read % 256;
        duty = ((a * (256 - frac)) + (b * frac));
        p->read += p->speed;
    }
    else {
        duty = 128*256; // silence
    }
    return duty;
}


static inline void playback_trigger(struct playback *p, uint32_t speed, uint32_t len, uint8_t *buf) {
    p->buf = buf;
    p->len = len;
    p->speed = speed;
    p->read = 0;
}

#ifdef LOG
static inline void playback_log(const char *tag, const struct playback *p) {
    LOG("%s:\n",tag);
    LOG("  read  = %x\n", p->read);
    LOG("  speed = %x\n", p->speed);
    LOG("  buf   = %x\n", p->buf);
    LOG("  len   = %x\n", p->len);
}
#endif


#endif
