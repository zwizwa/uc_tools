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


/* Parametric shift is inefficient on ARM. Check disassembly.
   Multiplication might actually be simpler.  I'm using this mostly on
   M3 so multiplication is fine.  */
//#define LEB128_SHIFT  // or multiply
//#define LEB128_SHIFT_PARAM // if shift, use parametric shift

/* Push tick method.  Return value is boolean indicating a number is
   ready in the accumulator. */
static inline void leb128_reset(struct leb128 *s) {
    s->accu = 0;
#ifdef LEB128_SHIFT
    s->segment = 0;
#else // multiply
    s->segment = 1;
#endif
}
static inline uint32_t leb128_decode_push(struct leb128 *s, uint8_t byte) {
#ifdef LEB128_SHIFT
#ifdef LEB128_SHIFT_PARAM
    uint32_t bits = (byte & 0x7f) << (s->segment * 7);
#else
    uint32_t bits = (byte & 0x7f);
    for(int i=s->segment; i; i--) bits <<= 7;
#endif
    s->segment++;
#else // multiply
    uint32_t bits = (byte & 0x7f) * s->segment;
    s->segment *= 128;
#endif
    s->accu |= bits;
    return 0 == (byte & 0x80);
}
static inline int32_t leb128_read(const uint8_t *buf, uint32_t nb_buf, uint32_t *val) {
    if (nb_buf == 0) return 0;
    struct leb128 s;
    leb128_reset(&s);
    uint32_t nb = 0;
    for(;;) {
        if(nb == nb_buf) {
            /* This is an error condition: the buffer contained a partial number. */
            return -1;
        }
        if (leb128_decode_push(&s, buf[nb++])) break;
    }
    if (val) *val = s.accu;
    return nb;
}
static inline int32_t leb128_tick(struct leb128 *s, uint8_t byte, int32_t (*push_word)(struct leb128 *s)) {
    int32_t rv;
    if ((rv = leb128_decode_push(s, byte))) {
        rv = push_word(s);
        leb128_reset(s);
    }
    return rv;
}


/* Assumes buffer is large enough. */
static inline uint32_t leb128_write(uint8_t *buf, uint32_t word) {
    uint32_t nb = 0;
    while(word > 0) {
        buf[nb] = (word <= 0x7f) ? word : ((word & 0x7f) | 0x80);
        word >>= 7;
        nb++;
    }
    return nb;
}


#endif
