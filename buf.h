#ifndef BUF_H
#define BUF_H

/* Circular byte buffer implemented as inline functions.
   Rolling pointers, power-of-2 size, wrap on access. */

/* Control codes. */
#define BUF_EAGAIN ((uint16_t)0x100)

struct buf {
    uint32_t write;
    uint32_t read;
    uint32_t mask;
    uint8_t *buf;
};


/* Note that size needs to be a power of two for this to work. */
static inline void buf_init(struct buf *b, uint8_t *buf, uint32_t size) {
    b->write = 0;
    b->read  = 0;
    b->mask  = size-1;
    b->buf   = buf;
}
static inline uint32_t buf_mask(struct buf *b, uint32_t index) {
    return b->mask;
}
static inline uint32_t buf_wrap(struct buf *b, uint32_t index) {
    return index & buf_mask(b, index);
}
static inline uint32_t buf_bytes(struct buf *b) {
    return b->write - b->read;
}
static inline int buf_empty(struct buf *b) {
    return 0 == buf_bytes(b);
}
static inline int buf_full(struct buf *b) {
    return b->mask == buf_bytes(b);
}
static inline void buf_put(struct buf *b, uint8_t byte) {
    if (!buf_full(b)) {
        b->buf[buf_wrap(b, b->write++)] = byte;
    }
}
static inline uint16_t buf_get(struct buf *b) {
    if (buf_empty(b)) return BUF_EAGAIN;
    return b->buf[buf_wrap(b, b->read++)];
}
static inline uint16_t buf_peek(struct buf *b, uint32_t offset) {
    if (offset > buf_bytes(b)) return BUF_EAGAIN;
    return b->buf[buf_wrap(b, b->read + offset)];
}
static inline void buf_drop(struct buf *b, uint32_t nb_drop) {
    uint32_t bs = buf_bytes(b);
    if (nb_drop > bs) { nb_drop = bs; }
    b->read += nb_drop;
}


#endif
