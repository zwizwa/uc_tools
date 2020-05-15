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

static inline uint8_t playback_next(struct playback *p) {
    uint32_t read_i = p->read >> 8;
    uint8_t duty;
    if (read_i < p->len) {
        if (p->buf) {
            duty = p->buf[read_i];
        }
        else {
            duty = (read_i % 256); // saw
        }
        p->read += p->speed;
    }
    else {
        duty = 128; // silence
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
