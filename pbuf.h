#ifndef PBUF_H
#define PBUF_H

#include <stdint.h>
#include <string.h>
#include "infof.h"

#include "cbuf.h"

/* Generic packet buffer. */
struct pbuf {
    uint32_t count;  /* Number of valid bytes in buffer */
    uint32_t size;   /* Total allocated size of buffer */
    uint8_t *buf;
};
#define PBUF_INIT_FROM_BUF(b) { .buf = &b[0], .size = sizeof(b), .count = sizeof(b) }

static inline void pbuf_init(struct pbuf *p, uint8_t *buf, uint32_t size) {
    p->count = 0;
    p->size  = size;
    p->buf   = buf;
    memset(buf, 0, size);
}

/* Initialize with statically allocated buffer with _buf postfix. */
#define PBUF_INIT(name) pbuf_init(&name, &name##_buf[0], sizeof(name##_buf))

static inline void pbuf_put(struct pbuf *p, uint8_t c) {
    if (p->count < p->size) {
        p->buf[p->count++] = c;
    }
}


typedef void (*pbuf_sink_t)(void*, const struct pbuf *p);


static inline uint64_t read_be(const uint8_t *buf, uint32_t nb) {
    uint64_t accu = 0;
    while(nb) {
        accu = (accu << 8) | (*buf++);
        nb--;
    }
    return accu;
}
static inline void write_be(uint8_t *buf, uint64_t word, uint32_t nb) {
    buf += nb;
    while(nb) {
        *(--buf) = word;
        word = word >> 8;
        nb--;
    }
}



/* Collector for incoming {packet,N} Erlang format.  Keep the length
   prefix in the buffer to avoid extra state tracking. */
static inline void pbuf_packetn_write(struct pbuf *p, int len_len,
                                      const uint8_t *buf, uint32_t len,
                                      pbuf_sink_t sink, void *ctx) {
    for (uint32_t i=0; i<len; i++) {
        pbuf_put(p, buf[i]);
        if (p->count >= len_len) {
            uint32_t count = read_be(&p->buf[0], len_len);
            if (p->count == count + 4) {
                if (sink) sink(ctx, p);
                p->count = 0;
            }
        }
    }
}

/* I'm at a loss for naming this properly, but the idea is that this
 * writes slip to a ciruclar buffer, collects data into a flat packet
 * buffer, and calls a function when packet is done.  It's a common
 * operation in slip-based boards.  FIXME: By re-arranging the calls
 * it is possible to keep the input buffer really short. */
static inline void slip_write_cp(
    const uint8_t *buf, uint32_t len, // input buffer
    struct cbuf *c,                   // stage 1 buffer for slip stream
    struct pbuf *p,                   // stage 2 buffer for decoded packet
    pbuf_sink_t sink, void *ctx) {    // handler of stage2 buffer

    /* When we read the cbuf immediately after writing, there can only
       ever be two characters in the buffer, which can be represented
       in a size 4 buffer -- one slot is never used to distinguish
       full from empty, and size needs to be power of two. */
    for(uint32_t i=0; i<len; i++) {
        cbuf_put(c, buf[i]);
        uint16_t fc;
        while (CBUF_EAGAIN != (fc = cbuf_get_slip_decode(c))) {
            if (CBUF_OOB(SLIP_END) == fc) {
                if (p->count) sink(ctx, p);
                p->count = 0;
            }
            else if (fc >= 0x100) {
                /* Out-of-band characters other than frame borders are not
                 * expected.  Not much that can be done here. */
                infof("oob %d\n", fc);
            }
            else {
                /* Normal characters get collected until buffer is full. */
                pbuf_put(p, fc);
            }
        }
    }
}


#endif

