#ifndef LEB128_H
#define LEB128_H

/* FIXME: This is just a doodle.  Context: Looking for a good way to
   abstract tag_u32 as a push-style machine, with LEB128 packet.  Do
   this with Johannes' stream abstraction. */

#include <stdint.h>


/* Write the inner routine as a push-style state machine taking bytes,
   so we can connect it to a push-style word machine. */
struct leb128 {
    uint32_t accu;
    uint32_t segment;
};


/* Push tick method.  Return value is boolean indicating a number is
   ready in the accumulator. */
static inline void leb128_reset(struct leb128 *s) {
    s->accu = 0;
    s->segment = 0;
}
static inline uint32_t leb128_decode_i32_push(struct leb128 *s, uint8_t byte) {
    uint32_t bits = (byte & 0x7f) << (s->segment * 7);
    s->segment++;
    s->accu |= bits;
    uint32_t last = 0 == (byte & 0x80);
    if (last && (byte & 0x40)) {
        s->accu -= (1 << (s->segment * 7));
    }
    return last;
}
static inline intptr_t leb128_read_i32(const uint8_t *buf, uintptr_t nb_buf, int32_t *val) {
    if (nb_buf == 0) return 0;
    struct leb128 s;
    leb128_reset(&s);
    uintptr_t nb = 0;
    for(;;) {
        if(nb == nb_buf) {
            /* This is an error condition: the buffer contained a
               partial number. */
            return -1;
        }
        if (leb128_decode_i32_push(&s, buf[nb++])) { break; }
    }
    /* Unsigned to signed conversion happens here. */
    if (val) *val = s.accu;
    return nb;
}
static inline int32_t leb128_tick(struct leb128 *s, uint8_t byte, int32_t (*push_word)(struct leb128 *s)) {
    int32_t rv;
    if ((rv = leb128_decode_i32_push(s, byte))) {
        rv = push_word(s);
        leb128_reset(s);
    }
    return rv;
}


/* Assumes buffer is large enough. */
static inline uintptr_t leb128_write_i32(uint8_t *buf, int32_t word) {
    uintptr_t nb = 0;
    for(;;) {
        if ((word >= -0x40) && (word <= 0x3f)) {
            // last
            buf[nb] = (word & 0x7f);
            nb++;
            return nb;
        }
        else {
            buf[nb] = (word & 0x7f) | 0x80;
            nb++;
            word >>= 7;
        }
    }
}


#endif
