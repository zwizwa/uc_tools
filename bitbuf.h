#ifndef BITBUF_H
#define BITBUF_H

/* Big endian bit buffer.

   Originally written for creating PWM signalling (e.g. 1-bit uart),
   but also useful for packing index arrays. */
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
static inline uint32_t bitbuf_flush(struct bitbuf *b) {
    uint32_t nbyte = b->count / 8;
    uint32_t nbit = b->count % 8;
    b->buf[nbyte] = b->shiftreg << (7 - nbit);
    return nbyte+1;
}
static inline void bitbuf_write(struct bitbuf *b, uint32_t bitval) {
    /* Precondition is that there is room in the shift register. */
    b->shiftreg = (b->shiftreg << 1) | bitval;
    uint32_t nbyte = b->count / 8;
    uint32_t nbit  = b->count % 8;
    if (nbit == 7) {
        b->buf[nbyte] = b->shiftreg;
    }
    b->count++;
}
static inline uint32_t bitbuf_read(struct bitbuf *b) {
    uint32_t nbyte = b->count / 8;
    uint32_t nbit  = b->count % 8;
    uint32_t bitval = 1 & (b->buf[nbyte] >> (7 - nbit));
    b->count++;
    return bitval;
}

#if 0
// FIXME: change rep to have a read and write pointer
static inline uint32_t bitbuf_read_word(struct bitbuf *b, uint32_t nb_bits) {
    uint32_t accu;
    while(nb_bits--) {
        accu =<< bitbuf_read(b);
    }
    return accu;
}
#endif

#endif
