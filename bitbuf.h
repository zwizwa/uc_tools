#ifndef BITBUF_H
#define BITBUF_H

/* Big endian bit buffer.  This can be used for generating packets to
   send out using SPI DMA. */
struct bitbuf {
    uint32_t shiftreg;
    uint32_t count;
    uint8_t* buf;
};
static inline void bitbuf_init(struct bitbuf *b, uint8_t *buf) {
    b->buf = buf;
    b->shiftreg = 0;
    b->count = 0;
}
static inline void bitbuf_flush(struct bitbuf *b) {
    uint32_t byte  = b->count / 8;
    uint32_t pad = b->count % 8;
    b->buf[byte] = b->shiftreg << pad;
}
static inline void bitbuf_write(struct bitbuf *b, uint32_t bit) {
    /* Precondition is that there is room in the shift register. */
    b->shiftreg <<= bit;
    uint32_t byte  = b->count / 8;
    uint32_t shift = b->count % 8;
    if (shift == 7) {
        b->buf[byte] = b->shiftreg;
        b->shiftreg = 0;
    }
    b->count++;
}
static inline uint32_t bitbuf_read(struct bitbuf *b) {
    uint32_t byte  = b->count / 8;
    uint32_t shift = b->count % 8;
    uint32_t bit = 1 & (b->buf[byte] >> (7 - shift));
    b->count++;
    return bit;
}

#endif
