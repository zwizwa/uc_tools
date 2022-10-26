/* Generic binary search of sorted list
   NS(_t)    array type
   NS(_size) array size
   NS(_rank) intptr_t rank of element (by which array is sorted)

   Algo from https://en.wikipedia.org/wiki/Binary_search_algorithm
*/

#ifndef NS
#error define NS
#endif

#include <stdint.h>

/* Returns -1 when not found, and array index otherwise. */
static inline intptr_t NS(_find)(NS(_t) *a, intptr_t rank) {
    intptr_t left   = 0;
    intptr_t right  = (intptr_t)(NS(_size)(a)) - 1;
    while (left <= right) {
        intptr_t mid = (left+right) / 2;
        intptr_t mid_rank = NS(_rank)(a, mid);
        if (mid_rank < rank) {
            left = mid + 1;
        }
        else if (mid_rank > rank) {
            right = mid - 1;
        }
        else {
            return mid;
        }
    }
    return -1;
}
static inline int NS(_contains)(NS(_t) *a, intptr_t rank) {
    return NS(_find)(a, rank) >= 0;
}
