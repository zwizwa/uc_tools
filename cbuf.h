#ifndef CBUF_H
#define CBUF_H

#include <stdint.h>
#include "slip.h"

/* Circular byte buffer implemented as inline functions.  Rolling
   pointers, power-of-2 size, wrap on access. */

/* Control codes. */
#define CBUF_EAGAIN ((uint16_t)0x100)

/* Only one word, much less headache. */
#define CBUF_WATERMARK 1
#define CBUF_COUNT_OVERFLOW 1

/* Relax the constraint to require power of two buffer sizes. */
#define CBUF_ARBITRARY_SIZE 1

struct cbuf {
    volatile uint32_t write;
    volatile uint32_t read;
    uint32_t size;
#ifdef CBUF_WATERMARK
    volatile uint32_t watermark;
#endif
#ifdef CBUF_COUNT_OVERFLOW
    volatile uint32_t overflow;
#endif
    volatile uint8_t *buf;
};

/* This is written for ARM Cortex3, which has atomic uint32_t
   read/write as long as the words are properly aligned, and no
   reordering of memory operations.

   This allows cbuf to be used as a lock-free queue.  I don't know
   what exactly I need to prove, but here are some arguments that
   ensure that reads and writes are consistent, i.e.:

   - the read/write pointers obtained by the reader always span a
     range of well-defined bytes.

   - the read/write pointers obtained by writer always span a range of
     discarded bytes.

   Conditions:

   - all pointers are strictly increasing.  this ensures that a
     pointer's motion has either been caught by the other end, or not.
     if it has moved but the motion was not noticed, an
     underestimation of the free space or full happens, but no
     inconsistency happens.  ( IMPLEMENTATION )

   - a single reader / single writer (multiple readers/writers violate
     atomic update). ( USE )

   - reads and writes are atomic: this ensures that the pointer read
     is always a value that has been written, not some mangled partial
     update. ( ARCHITECTURE )

   - reads and writes are ordered: e.g in the following cases:
     writer:  (write X) then (write Y)
     reader:  (read  Y) then (read  X)

     if (read Y) reads the value corresponding to (write Y) and not
     the previous one, then (read X) necessarily corresponds to the
     new value writen by (write X).
     ( ARCHITECTURE )

*/



/* Note that size needs to be a power of two for this to work. */
static inline void cbuf_init(struct cbuf *b, uint8_t *buf, uint32_t size) {
    b->write  = 0;
    b->read   = 0;
    b->size   = size;
    b->buf    = buf;
}

/* Initialize with statically allocated buffer with _buf postfix. */
#define CBUF_INIT(name) cbuf_init(&name, &name##_buf[0], sizeof(name##_buf))


static inline uint32_t cbuf_mask(struct cbuf *b) {
    return b->size - 1;
}
static inline uint32_t cbuf_wrap(struct cbuf *b, uint32_t index) {
#if CBUF_ARBITRARY_SIZE
    // return index % (b->sizem1+1);
    // This is used only internally, where there is maximally one
    // iteration, so don't do division.
    while(index >= b->size) index -= b->size;
    return index;
#else
    return index & cbuf_mask(b);
#endif
}
static inline uint32_t cbuf_bytes(struct cbuf *b) {
    return b->write - b->read;
}
// FIXME: is this - 1 still necessary?
static inline uint32_t cbuf_room(struct cbuf *b) {
    return b->size - 1 - cbuf_bytes(b);
}
static inline int cbuf_empty(struct cbuf *b) {
    return 0 == cbuf_bytes(b);
}
static inline int cbuf_full(struct cbuf *b) {
    return (b->size - 1) == cbuf_bytes(b);
}
static inline void cbuf_update_watermark(struct cbuf *b) {
#if CBUF_WATERMARK
    uint32_t bytes = cbuf_bytes(b);
    if (bytes > b->watermark) b->watermark = bytes;
#endif
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
static inline void cbuf_clear(struct cbuf *b) {
    cbuf_drop(b, 0xFFFFFFFF);
}

/* For last resort unhandled overflow debugging... */
#define CBUF_INFO_OVERFLOW 0

#if CBUF_INFO_OVERFLOW
#include "infof.h"
#endif

#define CBUF_V2 1
#if CBUF_V2

// FIXME: These two should probably be compiled routines, not static inline.

/* Write is a transaction.  Everything or nothing gets written. */
static inline uint32_t cbuf_write(struct cbuf *b, const uint8_t *buf, uint32_t len) {
    uint32_t read  = b->read;
    uint32_t write = b->write;
    uint32_t bytes = write - read;
    uint32_t room  = b->size - 1 - bytes;
    if (len > room) {
#if CBUF_COUNT_OVERFLOW
        b->overflow++;
#endif
#if CBUF_INFO_OVERFLOW
        infof("cbuf_write overflow %p %p %d %d\n", b, buf, len, room);
#endif
        return 0;
    }
    // FIXME: optimize to avoid computing wraparound
    for (uint32_t i=0; i<len; i++) {
        b->buf[cbuf_wrap(b, write+i)] = buf[i];
    }
    b->write = write + len;
    cbuf_update_watermark(b);
    return len;
}

/* Reads can't just be transactions as we don't know the size, so
 * reads always read the minimum of buffer space and bytes
 * available. */
static inline uint32_t cbuf_read(struct cbuf *b, uint8_t *buf, uint32_t len) {
    uint32_t read  = b->read;
    uint32_t write = b->write;
    uint32_t bytes = write - read;
    if (len > bytes) len = bytes;
    // FIXME: optimize to avoid computing wraparound
    for (uint32_t i=0; i<len; i++) {
        buf[i] = b->buf[cbuf_wrap(b, read+i)];
    }
    b->read = read + len;
    return len;
}
static inline uint16_t cbuf_get(struct cbuf *b) {
    uint8_t byte = 0;
    if (1 == cbuf_read(b, &byte, 1)) { return byte; }
    else return CBUF_EAGAIN;
}
static inline void cbuf_put(struct cbuf *b, uint8_t byte) {
    cbuf_write(b, &byte, 1);
}

#else
/* Old ad-hoc style.  This does not atomically write. */
static inline uint16_t cbuf_get(struct cbuf *b) {
    if (cbuf_empty(b)) return CBUF_EAGAIN;
    uint32_t read = b->read;
    uint16_t rv = b->buf[cbuf_wrap(b, read)];
    b->read = read+1;
    return rv;
}
static inline void cbuf_put(struct cbuf *b, uint8_t byte) {
    if (!cbuf_full(b)) {
        uint32_t write = b->write;
        b->buf[cbuf_wrap(b, write)] = byte;
        b->write = write+1;
    }
    update_watermark(b);
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
#endif


#define CBUF_WRITE(buf, ...) \
    do { uint8_t msg[] = __VA_ARGS__; cbuf_write(buf, msg, sizeof(msg)); } while(0)

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


struct slice {
    const void *buf;
    uint32_t len;
};
void cbuf_write_slip_slices(struct cbuf *b, const struct slice *buf, uint32_t n_slices);

/* It is otherwise such a pain... */
#define CBUF_WRITE_2(b, buf0, buf1) {           \
        struct slice slices[] = {               \
            {.buf = buf0, .len = sizeof(buf0)}, \
            {.buf = buf1, .len = sizeof(buf1)}  \
        };                                      \
        cbuf_write_slip_slices(b, slices, 2);   \
    }
#define CBUF_WRITE_3(b, buf0, buf1, buf2) { \
        struct slice slices[] = {                \
            {.buf = buf0, .len = sizeof(buf0)},  \
            {.buf = buf1, .len = sizeof(buf1)},  \
            {.buf = buf2, .len = sizeof(buf2)}   \
        };                                       \
        cbuf_write_slip_slices(b, slices, 3);    \
    }

void cbuf_write_slip_tagged(struct cbuf *b, uint16_t tag, const uint8_t *buf, uint32_t len);


#define CBUF_WRITE_SLIP(buf, ...) \
    do { uint8_t msg[] = __VA_ARGS__; cbuf_write_slip(buf, msg, sizeof(msg)); } while(0)


/* This isn't a great abstraction, but it's the best I have atm and it
   is quite useful.  The idea is to spill some abstract reader into a
   slip cbuf once there is some room.  This avoids "expanding"
   multiple slip messages. */
int cbuf_write_slip_from_read(
    struct cbuf *b,
    const uint8_t *hdr, uint32_t hdr_len,
    uint32_t buf_size,
    uint32_t (*read)(uint8_t *buf, uint32_t len));


#endif
