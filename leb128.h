#ifndef LEB128_H
#define LEB128_H

/* FIXME: This is just a doodle.  Context: Looking for a good way to
   abstract tag_u32 as a push-style machine, with LEB128 packet.  Do
   this with Johannes' stream abstraction. */

/* Write the inner routine as a push-style state machine taking bytes,
   so we can connect it to a push-style word machine. */
struct leb128 {
    uint32_t accu;
};
/* Push tick method.  Return value is boolean indicating a number is
   ready in the accumulator. */
static inline void leb128_reset(struct leb128 *s) {
    s->accu = 0;
}
static inline uint32_t leb128_push(struct leb128 *s, uint8_t byte) {
    s->accu = (s->accu << 7) | (byte & 0x7f);
    return (0 == byte & 0x80);
}
static inline int32_t leb128_read(const uint32_t *buf, uint32_t nb_buf, uint32_t *val) {
    if (nb_buf == 0) return 0;
    struct leb128 s;
    leb128_init(&s);
    uint32_t nb = 0;
    for(;;) {
        if(nb == nb_buf) {
            /* This is an error condition: the buffer contained a partial number. */
            return -1;
        }
        if (leb128_push(&s, buf[nb++])) break;
    }
    if (val) *val = s.accu;
    return nb;
}
static inline int32_t leb128_tick(struct leb128 *s, uint8_t byte, int32_t (*push_word)(struct lab128 *s)) {
    int32_t rv;
    if ((rv = leb128_push(s, byte))) {
        rv = push_word(s);
        lab128_reset(s);
    }
    return rv;
}


#endif
