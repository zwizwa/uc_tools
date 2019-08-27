#ifndef PBUF_H
#define PBUF_H

#include <stdint.h>

/* Generic packet buffer. */
struct pbuf {
    uint32_t count;
    uint32_t size;
    uint8_t *buf;
};
#define PBUF_INIT_FROM_BUF(b) { .buf = &b[0], .size = sizeof(b), .count = sizeof(b) }

static inline void pbuf_init(struct pbuf *p, uint8_t *buf, uint32_t size) {
    p->count = 0;
    p->size  = size;
    p->buf   = buf;
}

/* Initialize with statically allocated buffer with _buf postfix. */
#define PBUF_INIT(name) pbuf_init(&name, &name##_buf[0], sizeof(name##_buf))

static inline void pbuf_put(struct pbuf *p, uint32_t c) {
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



/* Collector for incoming {packet,2} Erlang format.  Keep the length
   prefix in * the buffer to avoid extra state tracking. */
static inline void pbuf_packetn_write(struct pbuf *p, int len_len,
                                      const uint8_t *buf, uint32_t len,
                                      pbuf_sink_t sink, void *ctx) {
    for (uint32_t i=0; i<len; i++) {
        pbuf_put(p, buf[i]);
        if (p->count >= len_len) {
            uint32_t count = read_be(buf, len_len);
            if (p->count == count) {
                if (sink) sink(ctx, p);
                p->count = 0;
            }
        }
    }
}




#endif

