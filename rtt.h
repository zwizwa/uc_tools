#ifndef RTT_H
#define RTT_H

#include <stdint.h>

/* up   = target->host
   down = host->target */
struct rtt_buf {
    const char* name;
    uint8_t *buf;
    uint32_t size;
    volatile uint32_t write; // offset
    volatile uint32_t read; // offset
    uint32_t flags;
};
struct rtt_hdr {
    char id[16]; /* "SEGGER RTT" */
    int32_t nb_up;
    int32_t nb_down;
};
/* Most common case: 1 up, 1 down */
struct rtt_1_1 {
    struct rtt_hdr hdr;
    struct rtt_buf buf[2];
};
#define RTT_BUF_INIT(_arr) { .buf = _arr, .size = sizeof(_arr) }
#define RTT_1_1_INIT(_up_buf, _down_buf) {                              \
        .hdr = { .id = "SEGGER RTT", .nb_up = 1, .nb_down = 1, },       \
        .buf = { RTT_BUF_INIT(_up_buf), RTT_BUF_INIT(_down_buf) },      \
  }

/* Re-use existing generic ring buffer code in ns_cbuf.h */
#define NS(name) rtt_buf##name
typedef struct rtt_buf rtt_buf_queue_t;
typedef uint8_t        rtt_buf_element_t;
typedef uint16_t       rtt_buf_oob_element_t;
static inline rtt_buf_oob_element_t rtt_buf_oob_element_none(void) { return 0xFFFF; }
#define NS_CBUF_DEBUG               0
#define NS_CBUF_DEBUG_INFO_OVERFLOW 0
#include "ns_cbuf.h"
#undef NS_CBUF_DEBUG
#undef NS_CBUF_DEBUG_INFO_OVERFLOW
#undef NS

static inline struct rtt_buf *rtt_up_buf(struct rtt_hdr *hdr, uint32_t up_buf) {
    struct rtt_buf *b0 = (void*)(&hdr[1]);
    return b0 + up_buf;
}
static inline struct rtt_buf *rtt_down_buf(struct rtt_hdr *hdr, uint32_t down_buf) {
    struct rtt_buf *b0 = (void*)(&hdr[1]);
    return b0 + hdr->nb_up + down_buf;
}
/* Target side */
static inline uint32_t rtt_target_up_write(struct rtt_hdr *hdr, uint32_t up_buf, const uint8_t *buf, uint32_t len) {
    return rtt_buf_write(rtt_up_buf(hdr, up_buf), buf, len);
}
static inline uint32_t rtt_target_down_read(struct rtt_hdr *hdr, uint32_t down_buf, uint8_t *buf, uint32_t len) {
    return rtt_buf_read(rtt_down_buf(hdr, down_buf), buf, len);
}
/* Host side */
static inline uint32_t rtt_host_down_write(struct rtt_hdr *hdr, uint32_t down_buf, const uint8_t *buf, uint32_t len) {
    return rtt_buf_write(rtt_down_buf(hdr, down_buf), buf, len);
}
static inline uint32_t rtt_host_up_read(struct rtt_hdr *hdr, uint32_t up_buf, uint8_t *buf, uint32_t len) {
    return rtt_buf_read(rtt_up_buf(hdr, up_buf), buf, len);
}


#endif

