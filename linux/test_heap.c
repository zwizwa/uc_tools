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


/* This is a test implementation with just integers. */

#include "macros.h"
#include <stdint.h>

typedef uint32_t heap_element_t;
typedef uint32_t heap_node_t;  // E.g. could be uint16_t
struct heap {
    int nb;
    heap_element_t *arr;  // 1 << nb_levels members
};
typedef struct heap heap_heap_t;
INLINE int heap_gte(heap_heap_t *h, uint32_t a, uint32_t b) { return a >= b; }

#define NS(name) heap##name
#include "ns_heap.h"
#undef NS


/* Have at least two instances to make sure that compiles.  This is
   not tested below, but can serve as an example of how to implement
   an indirect heap test using uint8_t indices.  This is not tested
   below. */
typedef uint8_t iheap_element_t;
struct indirect_element {
    uint32_t val;  // the value used in comparison
    uint32_t data; // some arbitrary attached payload, not used by heap algo.
};
struct iheap {
    int nb;
    iheap_element_t *arr;  // 1 << nb_levels members
    struct indirect_element *store;
};
typedef struct iheap iheap_heap_t;
/* We get passed iheap_element_t, which is a uint8_t index into the store. */
INLINE int iheap_gte(iheap_heap_t *h, iheap_element_t a, iheap_element_t b) {
    return h->store[a].val >= h->store[b].val;
}
typedef struct iheap iheap_heap_t;

#define NS(name) iheap##name
#include "ns_heap.h"
#undef NS



#include "xorshift.h"

void log_arr(uint32_t *arr, uint32_t nb) {
    for (int i=0; i<nb; i++) {  LOG(" %d", arr[i]); }
    LOG("\n");
}
void log_heap(struct heap *h) {
    log_arr(h->arr, h->nb);
}

void test1(int max_nb, int mul, int mod, int log) {
    uint32_t arr[max_nb];
    struct heap h = {
        .nb = 0, .arr = arr
    };
    for (int i=0; i<max_nb; i++) {
        int r = (mul ?  (i * mul) : random_u32()) % mod;
        heap_insert(&h, r);
        if (log) log_heap(&h);
    }
    if (log) LOG("\n");
    uint32_t last_top = 0xFFFFFFFF;
    for (int i=0; i<max_nb; i++) {
        uint32_t top = heap_pop(&h);
        if(log) {
            LOG(" %d |", top);
            log_heap(&h);
        }
        ASSERT(last_top >= top);
        last_top = top;
    }
}
void test2(int max_nb, int mul, int mod, int log) {
    uint32_t arr[max_nb];
    for (int i=0; i<max_nb; i++) {
        arr[i] = (i * mul) % mod;
    }
    log_arr(arr, max_nb);
    heap_sort(arr, max_nb);
    log_arr(arr, max_nb);
    uint32_t last = 0;
    for (int i=0; i<max_nb; i++) {
        ASSERT(last <= arr[i]);
        last = arr[i];
    }
}

int main(int argc, char **argv) {
    test1(10, 23, 71, 1);

    test1(20, 23, 71, 0);
    test1(20, 7,  13, 0);
    test2(20, 7,  13, 0);

    test1(10,    0, 23, 0);
    test1(100,   0, 2323, 0);
    test1(1000,  0, 232323, 0);
    test1(10000, 0, 23232323, 0);

    return 0;
}

