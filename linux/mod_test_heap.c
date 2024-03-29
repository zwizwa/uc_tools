#ifndef MOD_TEST_HEAP
#define MOD_TEST_HEAP

/* Heap data structure to implement a priority queue / software timer.

   See comments in ns_heap.h

   The algorithms uses the flat array representation of a binary tree
   as explained in [1].

   Children of node n are stored at 2n + 1 and 2n + 2 in te array.

   0 -> 1,2
   1 -> 3,4
   2 -> 5,6

   The agorithm works by incrementally restoring the heap property
   after each operation.  The heap property is expressed as:

   For each node n and all chuldren:  val(n) >= val(child(n))


   Some more references:

   [1] https://en.wikipedia.org/wiki/Heap_%28data_structure%29
   [2] https://en.wikipedia.org/wiki/Heapsort
   [3] https://stackoverflow.com/questions/16520607/maintaining-a-sorted-list
   [4] https://www.pjsip.org/pjlib/docs/html/group__PJ__TIMER.htm

   The main driver behind this abstraction is a software timer.
   See swtimer.h

*/

/* Toplevel can use mod_abort_longjmp.c
   Defaut is to use ABORT from macros.h */
#ifndef CATCH
#define CATCH if(0)
#endif

#ifndef TEST_LOG
#define TEST_LOG LOG
#endif

#include "macros.h"
#include <stdint.h>





/* For the test below, we just have a heap of numbers, without any
 * data attached. */

typedef uint32_t num_heap_element_t;
typedef uint32_t num_heap_node_t;
struct num_heap {
    int nb;
    num_heap_element_t *arr;
};
typedef struct num_heap num_heap_heap_t;
INLINE int num_heap_gte(num_heap_heap_t *h, uint32_t a, uint32_t b) { return a >= b; }
#define NS(name) num_heap##name
#include "ns_heap.h"
#undef NS


/* A more useful exemple, not tested below, but instantiated here to
   make sure it at least compiles.  This illustrates the idea of using
   the heap just to store indices into another data structure, and
   providing abstract dereference through the _gte method. */
typedef uint8_t ind_heap_element_t;
struct indirect_element {
    uint32_t val;  // the value used in comparison
    uint32_t data; // some arbitrary attached payload, not used by heap algo.
};
struct ind_heap {
    int nb;
    ind_heap_element_t *arr;  // 1 << nb_levels members
    struct indirect_element *store;
};
typedef struct ind_heap ind_heap_heap_t;
/* We get passed ind_heap_element_t, which is a uint8_t index into the store. */
INLINE int ind_heap_gte(ind_heap_heap_t *h, ind_heap_element_t a, ind_heap_element_t b) {
    return h->store[a].val >= h->store[b].val;
}
typedef struct ind_heap ind_heap_heap_t;
#define NS(name) ind_heap##name
#include "ns_heap.h"
#undef NS


#define XORSHIFT_STATIC 1
#include "xorshift.h"

void log_arr(uint32_t *arr, uint32_t nb) {
    for (int i=0; i<nb; i++) {  TEST_LOG(" %d", arr[i]); }
    TEST_LOG("\n");
}

/* If mul is 0, use a proper pseudo RNG with range limiting via mod.
   Otherwise use a "sawtooth" linear modulo cycle that provides
   unsorted numbers while retaining some predictability. */
uint32_t seq(int i, int mul, int mod) {
    return (mul ?  (i * mul) : random_u32()) % mod;
}

int heap_test1(int max_nb, int mul, int mod, int log) {
    CATCH { return 1; }

    uint32_t arr[max_nb];
    struct num_heap h = {
        .nb = 0, .arr = arr
    };
    for (int i=0; i<max_nb; i++) {
        int r = seq(i, mul, mod);
        num_heap_insert(&h, r);
        if (log) log_arr(h.arr, h.nb);
    }
    if (log) TEST_LOG("\n");
    uint32_t last_top = 0xFFFFFFFF;
    for (int i=0; i<max_nb; i++) {
        uint32_t top = num_heap_pop(&h);
        if(log) {
            TEST_LOG(" %d |", top);
            log_arr(h.arr, h.nb);
        }
        ASSERT(last_top >= top);
        last_top = top;
    }
    return 0;
}
void heap_test2(int max_nb, int mul, int mod, int log) {
    uint32_t arr[max_nb];
    for (int i=0; i<max_nb; i++) {
        arr[i] = seq(i, mul, mod);
    }
    log_arr(arr, max_nb);
    num_heap_sort(arr, max_nb);
    log_arr(arr, max_nb);
    uint32_t last = 0;
    for (int i=0; i<max_nb; i++) {
        ASSERT(last <= arr[i]);
        last = arr[i];
    }
}

/* Software timer. */
#include "swtimer.h"
void heap_test3(void) {
    swtimer_element_t arr[10];
    struct swtimer t = {
        .nb = 0, .arr = arr
    };
    /* Add 3 timeouts in random order. */
    swtimer_schedule(&t, 0x1000,0);
    swtimer_schedule(&t, 0xD000,0);
    swtimer_schedule(&t, 0x5000,0);
    /* Sequence expiration times. */
    swtimer_element_t next;
    while(swtimer_next(&t, &next)) {
        TEST_LOG("expire %x\n", next.time_abs);
    }
    uint16_t now = next.time_abs;
    swtimer_shift(&t, now);
    /* We're at D now, so D,E come before 1 and C */
    swtimer_schedule(&t, 0xCFFF-now,0);
    swtimer_schedule(&t, 0xD001-now,0);
    swtimer_schedule(&t, 0x1000-now,0);
    swtimer_schedule(&t, 0xE000-now,0);
    swtimer_schedule(&t, 0xD000-now,0);
    while(swtimer_next(&t, &next)) {
        TEST_LOG("expire %x\n", next.time_abs);
    }
}



#endif
