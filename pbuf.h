#ifndef PBUF_H
#define PBUF_H

#include <stdint.h>
#include <string.h>
#include "infof.h"

#include "cbuf.h"
#include "byteswap.h"

#define PBUF_DEBUG 0
#if PBUF_DEBUG
#define PBUF_WATERMARK 1
#define PBUF_COUNT_OVERFLOW 1
#else
#define PBUF_WATERMARK 0
#define PBUF_COUNT_OVERFLOW 0
#endif

/* Generic packet buffer. */
struct pbuf {
    uint32_t count;  /* Number of valid bytes in buffer */
    uint32_t size;   /* Total allocated size of buffer */
    uint8_t *buf;
#if PBUF_WATERMARK
    uint32_t watermark;
#endif
#if PBUF_COUNT_OVERFLOW
    uint32_t overflow;
#endif
};

/* A wrapper to keep track of how many bytes there are available in
   front of the current p.buf pointer.  This allows adjusting the pbuf
   to accomodate a header. */
struct pbuf_h {
    uint32_t header_room;
    struct pbuf p;
};


#define PBUF_INIT_FROM_BUF(b) { .buf = &b[0], .size = sizeof(b), .count = sizeof(b) }

static inline void pbuf_init(struct pbuf *p, uint8_t *buf, uint32_t size) {
    p->count = 0;
    p->size  = size;
    p->buf   = buf;
    memset(buf, 0, size);
}
static inline void pbuf_pool_init(struct pbuf *p, uint8_t *buf, uint32_t buf_size, uint32_t n_buf) {
    for(uint32_t i=0; i<n_buf; i++) {
        pbuf_init(&p[i], &buf[i * buf_size], buf_size);
    }
}

static inline void pbuf_clear(struct pbuf *p) {
    pbuf_init(p, p->buf, p->size);
}

static inline void pbuf_update_watermark(struct pbuf *p) {
#if PBUF_WATERMARK
    if (p->count > p->watermark) p->watermark = p->count;
#endif
}

/* Initialize with statically allocated buffer with _buf postfix. */
#define PBUF_INIT(name) pbuf_init(&name, &name##_buf[0], sizeof(name##_buf))

static inline void pbuf_put(struct pbuf *p, uint8_t c) {
    if (p->count < p->size) {
        p->buf[p->count++] = c;
    }
    else {
#if PBUF_COUNT_OVERFLOW
        p->overflow++;
#endif
    }
    pbuf_update_watermark(p);
}
static inline void pbuf_write(struct pbuf *p, const uint8_t *buf, uint32_t len) {
    for(uint32_t i=0; i<len; i++) {
        pbuf_put(p, buf[i]);
    }
}


typedef void (*pbuf_sink_t)(void*, const struct pbuf *p);



void pbuf_packetn_write(
    struct pbuf *p, int len_len,
    const uint8_t *buf, uint32_t len,
    pbuf_sink_t sink, void *ctx);

/* FIXME: This is not really necessary.   Use pbuf_slip_write_handle(). */
void pbuf_slip_write(
    const uint8_t *buf, uint32_t len, // input buffer
    struct cbuf *c,                   // stage 1 buffer for slip stream
    struct pbuf *p,                   // stage 2 buffer for decoded packet
    pbuf_sink_t sink, void *ctx);     // handler of stage2 buffer

void cbuf_write_slip_reply(
    struct cbuf *c,
    const struct pbuf *p,
    uint32_t offset,
    uint8_t *buf,
    uint32_t len);

/* pbuf + cbuf combo used for slip decoding. */
struct sbuf {
    struct pbuf p;
    struct cbuf c;
    uint8_t c_buf[4];
};
#define SBUF_INIT(name) \
    do { \
        pbuf_init(&name.p, &name##_buf[0], sizeof(name##_buf)); \
        cbuf_init(&name.c, &name.c_buf[0], sizeof(name.c_buf)); \
    } while(0)


/* The following two methods allow for a pbuf to be fed with a SLIP
   stream.  To do this, the byte at the write location is used as a
   1-byte buffer that potentially contains an escape code, which means
   that we do need to initialize it properly.  This effectively
   reduces the available space by 1 character. */
static inline void pbuf_slip_clear(struct pbuf *p) {
    pbuf_clear(p);
    p->buf[0] = 0;
}
static inline void pbuf_slip_put(struct pbuf *p, uint8_t c) {
    if (p->count >= p->size-1) return;
    if (SLIP_END == c) return;

    uint8_t last_was_escape = p->buf[p->count];
    if (last_was_escape) {
        if      (SLIP_ESC_ESC == c) { c = SLIP_ESC; }
        else if (SLIP_ESC_END == c) { c = SLIP_END; }
        p->buf[p->count++] = c;
        p->buf[p->count] = 0; // not escape
    }
    else {
        if (SLIP_ESC == c) {
            p->buf[p->count] = 1; // escape
        }
        else {
            p->buf[p->count++] = c;
            p->buf[p->count] = 0; // not escape
        }
    }
}
static inline void pbuf_slip_for(
    struct pbuf *p,
    const uint8_t *buf, uint32_t len,
    void (*handle)(struct pbuf *)) {

    for(uint32_t i=0; i<len; i++) {
        if(SLIP_END == buf[i]) {
            handle(p);
            pbuf_slip_clear(p);
        }
        else {
            pbuf_slip_put(p, buf[i]);
        }
    }
}





#endif

