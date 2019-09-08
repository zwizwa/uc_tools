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

void pbuf_packetn_write(
    struct pbuf *p, int len_len,
    const uint8_t *buf, uint32_t len,
    pbuf_sink_t sink, void *ctx);

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





#endif

