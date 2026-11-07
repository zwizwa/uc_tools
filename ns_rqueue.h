/* Lock-free queue of variable length records, one producer, one
   consumer.  Same arguments as ns_cbuf.h: each index has a single
   writer and only moves forward, record contents are complete before
   the index that publishes them moves.

   - records are {uint16_t len, uint16_t tag, payload}, 4 byte aligned
   - a record never wraps: the rest of the buffer becomes a pad record
   - reserve/commit and peek/release, so data can be built or used in
     place
   - size must be a power of two, read/write are free running

   User defines NS(), and NS(_queue_t) with these fields:
     volatile uint32_t write;  // producer
     volatile uint32_t read;   // consumer
     uint32_t size;
     uint8_t *buf;             // 4 byte aligned
     uint32_t reserved;        // producer private
*/

#include <stdint.h>

#ifndef NS_RQUEUE_PAD
#define NS_RQUEUE_PAD 0xFFFF
#define NS_RQUEUE_HDR 4
#endif

#ifndef NS_RQUEUE_BARRIER
#define NS_RQUEUE_BARRIER() __asm__ volatile ("" ::: "memory")
#endif

static inline void NS(_init)(NS(_queue_t) *q, uint8_t *buf, uint32_t size) {
    q->write = 0;
    q->read = 0;
    q->size = size;
    q->buf = buf;
    q->reserved = 0;
}
static inline int NS(_empty)(const NS(_queue_t) *q) {
    return q->write == q->read;
}
static inline uint32_t NS(_align)(uint32_t len) {
    return (len + 3) & ~3;
}
static inline void NS(_hdr)(NS(_queue_t) *q, uint32_t off, uint16_t len, uint16_t tag) {
    uint16_t *h = (uint16_t *)(q->buf + off);
    h[0] = len;
    h[1] = tag;
}

/* Producer: returns payload pointer, or 0 when full. */
static inline uint8_t *NS(_reserve)(NS(_queue_t) *q, uint16_t tag, uint32_t len) {
    uint32_t need = NS_RQUEUE_HDR + NS(_align)(len);
    uint32_t off  = q->write & (q->size - 1);
    uint32_t pad  = off + need > q->size ? q->size - off : 0;
    uint32_t used = q->write - q->read;
    if (need > q->size || used + pad + need > q->size) return 0;
    if (pad) {
        NS(_hdr)(q, off, pad - NS_RQUEUE_HDR, NS_RQUEUE_PAD);
        off = 0;
    }
    NS(_hdr)(q, off, len, tag);
    q->reserved = pad + need;
    return q->buf + off + NS_RQUEUE_HDR;
}
static inline void NS(_commit)(NS(_queue_t) *q) {
    NS_RQUEUE_BARRIER();
    q->write += q->reserved;
    q->reserved = 0;
}

/* Consumer: returns payload pointer and fills in tag and len, or
   returns 0 when empty.  Skips pad records. */
static inline uint8_t *NS(_peek)(NS(_queue_t) *q, uint16_t *tag, uint16_t *len) {
    for (;;) {
        if (NS(_empty)(q)) return 0;
        NS_RQUEUE_BARRIER();
        uint32_t off = q->read & (q->size - 1);
        uint16_t *h = (uint16_t *)(q->buf + off);
        if (h[1] == NS_RQUEUE_PAD) {
            q->read += q->size - off;
            continue;
        }
        *len = h[0];
        *tag = h[1];
        return q->buf + off + NS_RQUEUE_HDR;
    }
}
static inline void NS(_release)(NS(_queue_t) *q) {
    uint16_t *h = (uint16_t *)(q->buf + (q->read & (q->size - 1)));
    uint32_t need = NS_RQUEUE_HDR + NS(_align)(h[0]);
    NS_RQUEUE_BARRIER();
    q->read += need;
}
