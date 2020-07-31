#ifndef CBUF_H
#define CBUF_H

#include <stdint.h>
#include "slip.h"
#include "macros.h"

/* Circular byte buffer implemented as inline functions.  Rolling
   pointers, power-of-2 size, wrap on access. */

/* Control codes. */
#define CBUF_EAGAIN ((uint16_t)0x100)

/* Debug config. */
#define CBUF_DEBUG 1
#if CBUF_DEBUG
#define CBUF_WATERMARK 1
#define CBUF_COUNT_OVERFLOW 1
#else
#define CBUF_WATERMARK 0
#define CBUF_COUNT_OVERFLOW 0
#endif

/* For last resort unhandled overflow debugging... */
#define CBUF_INFO_OVERFLOW 0

/* Relax the constraint to require power of two buffer sizes.  This is
   now the default, but left here for backward compatibility. */
#define CBUF_ARBITRARY_SIZE 1

struct cbuf {
    volatile uint32_t write;
    volatile uint32_t read;
    uint32_t size;
    volatile uint8_t *buf;
#ifdef CBUF_WATERMARK
    volatile uint32_t watermark;
#endif
#ifdef CBUF_COUNT_OVERFLOW
    volatile uint32_t overflow;
#endif
};

/* Typedefs and macros for ns_queue.h using the cbuf namespace prefix. */
typedef struct cbuf cbuf_queue_t;
typedef uint8_t cbuf_element_t;
typedef uint16_t cbuf_fat_element_t;

#define cbuf_none CBUF_EAGAIN

/* Most of the functionality is inherited from the generic circular
   bufer in ns_cbuf.h

   Note that the namespace prefix needs to be generated using CONCAT
   and not just ##, becuase we're concatenating macro names in
   ns_cbuf.h for NS(_none) */
#define NS(name) CONCAT(cbuf,name)
#include "ns_cbuf.h"
#undef NS


/* The rest is specific to byte buffers. */

/* Initialize with statically allocated buffer with _buf postfix. */
#define CBUF_INIT(name) cbuf_init(&name, &name##_buf[0], sizeof(name##_buf))

/* Backwards compatibility. */
static inline uint32_t cbuf_bytes(const struct cbuf *b) {
    return cbuf_elements(b);
}

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
