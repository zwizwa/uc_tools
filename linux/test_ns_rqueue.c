/* Randomized single-threaded test of ns_rqueue.h: interleaves
   reserve/commit and peek/release with random record sizes in a small
   buffer, checks FIFO order, tags, payloads, that records never wrap,
   and that pad records occur.

   make linux/test_ns_rqueue.dynamic.host.elf
   ./linux/test_ns_rqueue.dynamic.host.elf */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include "macros.h"

struct tq {
    volatile uint32_t write;
    volatile uint32_t read;
    uint32_t size;
    uint8_t *buf;
    uint32_t reserved;
};
typedef struct tq tq_queue_t;
#define NS(name) tq##name
#include "ns_rqueue.h"
#undef NS

#define NB_LEN (1 << 16)  // > max records in flight

int main(void) {
    static uint8_t buf[256] __attribute__((aligned(4)));
    static uint32_t lens[NB_LEN];
    struct tq q;
    tq_init(&q, buf, sizeof(buf));
    uint32_t prod = 0, cons = 0, full = 0, pads = 0;
    srand(1);
    for (int step = 0; step < 2000000; step++) {
        if (rand() & 1) {
            uint32_t len = rand() % 120;
            uint32_t off = q.write & (q.size - 1);
            uint8_t *d = tq_reserve(&q, prod & 0x7fff, len);
            if (!d) { full++; continue; }
            if (d == buf + NS_RQUEUE_HDR && off != 0) pads++;
            for (uint32_t i = 0; i < len; i++) d[i] = prod + i;
            tq_commit(&q);
            lens[prod % NB_LEN] = len;
            prod++;
        }
        else {
            uint16_t tag, len;
            uint8_t *d = tq_peek(&q, &tag, &len);
            if (!d) continue;
            ASSERT(tag == (cons & 0x7fff));
            ASSERT(len == lens[cons % NB_LEN]);
            ASSERT(d + len <= buf + sizeof(buf));
            for (uint32_t i = 0; i < len; i++) ASSERT(d[i] == (uint8_t)(cons + i));
            tq_release(&q);
            cons++;
        }
    }
    /* Too big for the buffer */
    ASSERT(!tq_reserve(&q, 0, sizeof(buf)));
    ASSERT(pads > 0);
    LOG("ok produced=%d consumed=%d full=%d pads=%d\n", prod, cons, full, pads);
    return 0;
}
