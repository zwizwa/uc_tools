#ifndef SWTIMER_H
#define SWTIMER_H

/* 16 bit software timer. */

typedef struct swtimer_element {
    uint16_t time;
    uint16_t tag;
} swtimer_element_t;

typedef struct swtimer {
    int nb;
    swtimer_element_t *arr;
    uint16_t timebase;
} swtimer_heap_t;

static inline int swtimer_gte(swtimer_heap_t *h,
                              swtimer_element_t a,
                              swtimer_element_t b) {
    // Note:
    // - Inverted: We want a min-heap, while ns_heap is a max-heap
    // - The timebase allows correct interpretation of wraparound
    uint16_t delta_a = a.time - h->timebase;
    uint16_t delta_b = b.time - h->timebase;
    return delta_a <= delta_b;
}

/* Defines swtimer_insert() and swtimer_pop() */
#define NS(name) swtimer##name
#include "ns_heap.h"
#undef NS

static inline int swtimer_next(swtimer_heap_t *h, swtimer_element_t *next) {
    if (h->nb == 0) return 0;
    // The smallest value can now be used as timebase for future time
    // comparisons.
    *next = swtimer_pop(h);
    h->timebase = next->time;
    return 1;
}

static inline void swtimer_schedule(swtimer_heap_t *h, uint16_t time, uint16_t tag) {
    swtimer_element_t e = {time, tag};
    return swtimer_insert(h, e);
}

/* To cancel a timer, it might be simplest to set a bit in a flag
   register that will drop the software event when the hardware timer
   expires.  Resetting the hardware timer is something that needs a
   little thought as it needs to be done atomically. */

#endif

