#ifndef HEAP_H
#define HEAP_H

#include "macros.h"
#include <stdint.h>

/* Heap data structure to implement a priority queue / software timer.

   [1] https://en.wikipedia.org/wiki/Heap_%28data_structure%29
   [2] https://en.wikipedia.org/wiki/Heapsort
   [3] https://stackoverflow.com/questions/16520607/maintaining-a-sorted-list
   [4] https://www.pjsip.org/pjlib/docs/html/group__PJ__TIMER.htm
*/


/* Use case: software timer that delivers timeout tokes to a set of
   buffers.  My primary use for this is receive timeouts, so timer
   interrupt will run at the same priority as receivers to avoid
   synchronization issues.

   For a timer, "less than" should probably be defined circularly, so
   rolling counters can be used.
*/

/* TODO:
   - size and resulution.
   - attached properties
*/

/* Heap property: parent >= children */

/* Uses the flat array representation of a binary tree as explained in [1].

   children of n are at 2n + 1 and 2n + 2

   0 -> 1,2
   1 -> 3,4
   2 -> 5,6

   etc */


struct heap {
    uint32_t nb, max_nb;
    uint32_t *buf;  // 1 << nb_levels members
};
INLINE uint32_t heap_left(int n)   { return 2*n + 1; }
INLINE uint32_t heap_right(int n)  { return 2*n + 2; }
INLINE uint32_t heap_parent(int n) { return (n-1)/2; }
INLINE uint32_t heap_ordered(struct heap *h, int p, int c) {
    /* While we do require that p exists, also handle the case where
       the child does not exist.  This makes the rest of the code
       easier to express. */
    if (c >= h->nb) return 1;
    return h->buf[p] >= h->buf[c];
}
INLINE void heap_swap(struct heap *h, int a, int b) {
    uint32_t tmp = h->buf[a];
    h->buf[a] = h->buf[b];
    h->buf[b] = tmp;
}


/* Inserting at bottom and bubble up the property. */
INLINE void heap_insert_bottom(struct heap *h, uint32_t e) {
    int n = h->nb++;
    h->buf[n] = e;
    for(;;) {
        uint32_t p = heap_parent(n);
        if (heap_ordered(h, p, n)) {
            /* Restored. */
            break;
        }
        heap_swap(h, n, p);
        n = p;
        if (n == 0) {
            /* At the top node. */
            break;
        }
    }
}

/* Replace element at top and bubble down the property. */
INLINE void heap_replace_top(struct heap *h, uint32_t e) {
    h->buf[0] = e;
    int n = 0;
    for(;;) {
        uint32_t l = heap_left(n);
        uint32_t r = heap_right(n);

        /* Stop condition: no more children */
        if ((l >= h->nb) && (r >= h->nb)){
            break;
        }
        /* Bubbling down there are 3 elements at play.  If the current
           node is ordered, we're done. */
        if (heap_ordered(h, n, l) &&
            heap_ordered(h, n, r)) {
            break;
        }
        /* Otherwise, pick the largest element.  At least one of them
           exists, so because of the loop order we know that the left
           one exists, and so can use the comparison operation that
           allows the right one to be missing. */
        if (heap_ordered(h, l, r)) {
            heap_swap(h, n, l); n = l;
        }
        else {
            /* If r is larger, it is guaranteed to exist. */
            heap_swap(h, n, r); n = r;
        }
        /* n is now the subtree that needs to be checked and is
           guaranteed to exist.  */
    }
}
INLINE uint32_t heap_pop(struct heap *h) {
    uint32_t top = h->buf[0];
    h->nb--;
    if (h->nb > 0) { heap_replace_top(h, h->buf[h->nb]); }
    return top;
}
/* Trivial implementation.  Not optimal. */
INLINE void heap_sort(uint32_t *buf, uint32_t len) {
    struct heap h = { .nb = 0, .max_nb = len, .buf = buf };
    for (uint32_t i=0; i<len; i++) {
        heap_insert_bottom(&h, buf[i]);
    }
    for (uint32_t i=0; i<len; i++) {
        buf[len-1-i] = heap_pop(&h);
    }
}


#endif
