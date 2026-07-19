/* Generic binary search of sorted list
   NS(_t)    array type
   NS(_size) array size
   NS(_rank) intptr_t rank of element (by which array is sorted)

   Algo from https://en.wikipedia.org/wiki/Binary_search_algorithm

   Claude generalizations
   https://claude.ai/chat/a3b98196-bf4e-473b-8432-1cc9240fc9b6

   See test_bisect.c
*/

#ifndef NS
#error define NS
#endif

#include <stdint.h>

/* Returns -1 when not found, and array index otherwise. */
static inline intptr_t NS(_find)(NS(_t) *a, intptr_t rank) {
    intptr_t left   = 0;
    intptr_t right  = (intptr_t)(NS(_size)(a)) - 1; /* inclusive */
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

// Rest is Claude generalizing to find the first/last matching element
// using full bisection: instead of returning mid when equal, just
// keep going. It replaced the inclusive right with an exclusive one.
//


/* Returns index of the FIRST element with the given rank,
   or -1 when not found. */
#if 0
static inline intptr_t NS(_find_first)(NS(_t) *a, intptr_t rank) {
    intptr_t left  = 0;
    intptr_t right = (intptr_t)(NS(_size)(a));   /* exclusive */
    while (left < right) {
        intptr_t mid = left + (right - left) / 2;
        if (NS(_rank)(a, mid) < rank) {
            left = mid + 1;
        }
        else {
            right = mid;                          /* even on == */
        }
    }
    if (left < (intptr_t)(NS(_size)(a)) && NS(_rank)(a, left) == rank)
        return left;
    return -1;
}

#else
/* First index with rank >= target; equals size when all ranks are smaller.
   This is the insertion point that keeps the array sorted. */
static inline intptr_t NS(_lower_bound)(NS(_t) *a, intptr_t rank) {
    intptr_t left  = 0;
    intptr_t right = (intptr_t)(NS(_size)(a));
    while (left < right) {
        intptr_t mid = left + (right - left) / 2;
        if (NS(_rank)(a, mid) < rank) {
            left = mid + 1;
        }
        else {
            right = mid;
        }
    }
    return left;
}
static inline intptr_t NS(_find_first)(NS(_t) *a, intptr_t rank) {
    intptr_t i = NS(_lower_bound)(a, rank);
    if (i < (intptr_t)(NS(_size)(a)) && NS(_rank)(a, i) == rank)
        return i;
    return -1;
}
/* First index with rank > target (i.e. one past the last match). */
static inline intptr_t NS(_upper_bound)(NS(_t) *a, intptr_t rank) {
    intptr_t left  = 0;
    intptr_t right = (intptr_t)(NS(_size)(a));
    while (left < right) {
        intptr_t mid = left + (right - left) / 2;
        if (NS(_rank)(a, mid) <= rank) {    /* only change: <= */
            left = mid + 1;
        }
        else {
            right = mid;
        }
    }
    return left;
}
static inline intptr_t NS(_find_last)(NS(_t) *a, intptr_t rank) {
    intptr_t i = NS(_upper_bound)(a, rank) - 1;
    if (i >= 0 && NS(_rank)(a, i) == rank) {
        return i;
    }
    return -1;
}
#endif
