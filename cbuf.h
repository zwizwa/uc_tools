#ifndef CBUF_H
#define CBUF_H

#include <stdint.h>

/* Circular byte buffer implemented as inline functions.
   Rolling pointers, power-of-2 size, wrap on access. */

/* Control codes. */
#define CBUF_EAGAIN ((uint16_t)0x100)

struct cbuf {
    volatile uint32_t write;
    volatile uint32_t read;
    uint32_t mask;
    uint8_t *buf;
};

/* FIXME: With read/write volatile, it might be best to manually
 * inline some of these operations such that they do not cause
 * multiple reads. */


/* Note that size needs to be a power of two for this to work. */
static inline void cbuf_init(struct cbuf *b, uint8_t *buf, uint32_t size) {
    b->write = 0;
    b->read  = 0;
    b->mask  = size-1;
    b->buf   = buf;
}

/* Initialize with statically allocated buffer with _buf postfix. */
#define CBUF_INIT(name) cbuf_init(&name, &name##_buf[0], sizeof(name##_buf))


static inline uint32_t cbuf_mask(struct cbuf *b, uint32_t index) {
    return b->mask;
}
static inline uint32_t cbuf_wrap(struct cbuf *b, uint32_t index) {
    return index & cbuf_mask(b, index);
}
static inline uint32_t cbuf_bytes(struct cbuf *b) {
    return b->write - b->read;
}
static inline uint32_t cbuf_room(struct cbuf *b) {
    return b->mask - cbuf_bytes(b);
}
static inline int cbuf_empty(struct cbuf *b) {
    return 0 == cbuf_bytes(b);
}
static inline int cbuf_full(struct cbuf *b) {
    return b->mask == cbuf_bytes(b);
}
static inline void cbuf_put(struct cbuf *b, uint8_t byte) {
    if (!cbuf_full(b)) {
        b->buf[cbuf_wrap(b, b->write++)] = byte;
    }
}
static inline uint16_t cbuf_get(struct cbuf *b) {
    if (cbuf_empty(b)) return CBUF_EAGAIN;
    return b->buf[cbuf_wrap(b, b->read++)];
}
static inline uint16_t cbuf_peek(struct cbuf *b, uint32_t offset) {
    if (offset >= cbuf_bytes(b)) return CBUF_EAGAIN;
    return b->buf[cbuf_wrap(b, b->read + offset)];
}
static inline void cbuf_drop(struct cbuf *b, uint32_t nb_drop) {
    uint32_t bs = cbuf_bytes(b);
    if (nb_drop > bs) { nb_drop = bs; }
    b->read += nb_drop;
}
static inline void cbuf_write(struct cbuf *b, const uint8_t *buf, uint32_t len) {
    for (uint32_t i=0; i<len; i++) {
        cbuf_put(b, buf[i]);
    }
}
static inline uint32_t cbuf_read(struct cbuf *b, uint8_t *buf, uint32_t len) {
    uint32_t i = 0;
    while ((i < len) && (!cbuf_empty(b))) {
        buf[i++] = cbuf_get(b);
    }
    return i;
}


/* SLIP */
#define SLIP_END     0xC0 // 192 Packet separation marker (Break/MAB start)
#define SLIP_ESC     0xDB // 219 Escape character
#define SLIP_ESC_END 0xDC // 220 Re-mapped END, after ESC
#define SLIP_ESC_ESC 0xDD // 221 Re-mapped ESC, after ESC

#define CBUF_OOB_BASE 0x200

/* This is a macro so it can be used as a "case" */
#define CBUF_OOB(code) (CBUF_OOB_BASE | ((code) & 0xFF))

/* Provide an out-of-band interface to slip-encoded characters. */
uint16_t cbuf_peek_slip_decode(struct cbuf *b, uint32_t *nb_drop);
uint16_t cbuf_get_slip_decode(struct cbuf *b);

void cbuf_put_slip(struct cbuf *b, uint16_t fc);
void cbuf_write_slip(struct cbuf *b, uint8_t *buf, uint32_t len);


#endif
