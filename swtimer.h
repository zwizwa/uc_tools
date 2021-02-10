#ifndef SWTIMER_H
#define SWTIMER_H

/* 16 bit software timer. */

typedef struct swtimer_element {
    uint16_t time_abs;
    uint16_t tag;
} swtimer_element_t;

typedef struct swtimer {
    int nb;
    swtimer_element_t *arr;
    uint16_t now_abs;
} swtimer_heap_t;

static inline int swtimer_gte(swtimer_heap_t *h,
                              swtimer_element_t a,
                              swtimer_element_t b) {
    // Note:
    // - Inverted: We want a min-heap, while ns_heap is a max-heap
    // - The now_abs marker allows correct interpretation of wraparound
    uint16_t delta_a = a.time_abs - h->now_abs;
    uint16_t delta_b = b.time_abs - h->now_abs;
    return delta_a <= delta_b;
}

/* Defines swtimer_insert() and swtimer_pop() */
#define NS(name) swtimer##name
#include "ns_heap.h"
#undef NS

/* Update the time base only when _actual_ time has elapsed. */
static inline void swtimer_shift(swtimer_heap_t *h, uint16_t now_abs) {
    h->now_abs = now_abs;
}

static inline int swtimer_next(swtimer_heap_t *h, swtimer_element_t *next) {
    if (h->nb == 0) return 0;
    *next = swtimer_pop(h);
    return 1;
}

static inline void swtimer_schedule(swtimer_heap_t *h,
                                    uint16_t time_rel, uint16_t tag) {
    swtimer_element_t e = {h->now_abs + time_rel, tag};
    return swtimer_insert(h, e);
}

/* Precondition: at least one element in the heap. */
static inline uint16_t swtimer_diff(swtimer_heap_t *h) {
    swtimer_element_t next = swtimer_peek(h);
    uint16_t diff = next.time_abs - h->now_abs;
    return diff;
}

/* To cancel a timer, it might be simplest to set a bit in a flag
   register that will drop the software event when the hardware timer
   expires.  Resetting the hardware timer is something that needs a
   little thought as it needs to be done atomically. */

#endif

