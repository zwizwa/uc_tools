/* Heap data structure.

   [1] https://en.wikipedia.org/wiki/Heap_%28data_structure%29
   [2] https://en.wikipedia.org/wiki/Heapsort
*/

#ifndef NS
#error define NS
#endif

/* Dependencies:
   NS(_heap_t) with .nb and .arr members
   NS(_element_t)
   NS(_gte)

   - The array used to store the heap is a concrate C array,
     implemented as a struct with "nb" and "arr" members.  Any
     abstract indirection can be implemented in NS(_element_t) and
     NS(_gte).

   - Size is not checked.  It will need to be ensured externally that
     indices fit the array.  This keeps the error handling out of this
     file.

*/

#include "macros.h"
#include <stdint.h>


INLINE int NS(_left)(int n)   { return 2*n + 1; }
INLINE int NS(_right)(int n)  { return 2*n + 2; }
INLINE int NS(_parent)(int n) { return (n-1)/2; }
INLINE int NS(_ordered)(NS(_heap_t) *h, int p, int c) {
    /* While we do require that p exists, also handle the case where
       the child does not exist.  This makes the rest of the code
       easier to express. */
    if (c >= h->nb) return 1;
    return NS(_gte)(h, h->arr[p], h->arr[c]);
}
INLINE void NS(_swap)(NS(_heap_t) *h, int a, int b) {
    NS(_element_t) tmp = h->arr[a];
    h->arr[a] = h->arr[b];
    h->arr[b] = tmp;
}


/* Inserting at bottom and bubble up the property. */
INLINE void NS(_insert)(NS(_heap_t) *h, NS(_element_t) e) {
    int n = h->nb++;
    h->arr[n] = e;
    for(;;) {
        uint32_t p = NS(_parent)(n);
        if (NS(_ordered)(h, p, n)) {
            /* Restored. */
            break;
        }
        NS(_swap)(h, n, p);
        n = p;
        if (n == 0) {
            /* At the top node. */
            break;
        }
    }
}

/* Replace element at top and bubble down the property. */
INLINE void NS(_replace_top)(NS(_heap_t) *h, NS(_element_t) e) {
    h->arr[0] = e;
    int n = 0;
    for(;;) {
        uint32_t l = NS(_left)(n);
        uint32_t r = NS(_right)(n);

        /* Stop condition: no more children */
        if ((l >= h->nb) && (r >= h->nb)){
            break;
        }
        /* Bubbling down there are 3 elements at play.  If the current
           node is ordered, we're done. */
        if (NS(_ordered)(h, n, l) &&
            NS(_ordered)(h, n, r)) {
            break;
        }
        /* Otherwise, pick the largest element.  At least one of them
           exists, so because of the loop order we know that the left
           one exists, and so can use the comparison operation that
           allows the right one to be missing. */
        if (NS(_ordered)(h, l, r)) {
            NS(_swap)(h, n, l); n = l;
        }
        else {
            /* If r is larger, it is guaranteed to exist. */
            NS(_swap)(h, n, r); n = r;
        }
        /* n is now the subtree that needs to be checked and is
           guaranteed to exist.  */
    }
}
INLINE NS(_element_t) NS(_peek)(NS(_heap_t) *h) {
    return h->arr[0];
}
INLINE NS(_element_t) NS(_pop)(NS(_heap_t) *h) {
    NS(_element_t) top = NS(_peek)(h);
    h->nb--;
    if (h->nb > 0) { NS(_replace_top)(h, h->arr[h->nb]); }
    return top;
}
/* This is not optimal but might be useful anyway.  The main purpose
   for ns_heap.h is to implement a priority queue.  Implementing the
   following sub-optimal sort routine is as good as free, but an
   optimal implementation would require additional primitivies. */
INLINE void NS(_sort)(NS(_element_t) *arr, uint32_t len) {
    NS(_heap_t) h = { .nb = 0, .arr = arr};
    for (uint32_t i=0; i<len; i++) {
        NS(_insert)(&h, arr[i]);
    }
    for (uint32_t i=0; i<len; i++) {
        arr[len-1-i] = NS(_pop)(&h);
    }
}

