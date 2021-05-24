#ifndef LOG_INDEX_H
#define LOG_INDEX_H

/* Goal: I want to have signal / wave form display with event
   annotation.  The data structure used for event recording is a
   linear list of time-stamped events, together with an offset index
   that can be used for bisection search of a time stamp.

   This file is currently specialized to compute an index for an
   ad-hoc logging format: ASCII logs with circular 32 bit hex time
   stamps, interspersed with an ad-hoc binary format with 32 bit
   binary time stamps.
*/

/* API: Input uses a memory interface to keep the scanner fast.  This
   could be a memory-mapped file.  The output is abstract to allow
   caller to perform allocation. */

// FIXME: convert 32 bit timestamp to 64 bit.

#include "macros.h"
#include <stdint.h>

static inline void log_index_init(
    void (*index_append)(uintptr_t offset),
    const uint8_t *log, uintptr_t log_size) {

    uintptr_t i = 0, last = 0;
    for(;;) {
        uint8_t b = log[i];
        if (b & 0x80) {
            /* Time-stamped binary message. */
            uintptr_t len = 1 + 4 + (b & 0x7f);
            i += len;
            index_append(i);
            last = i;
        }
        else if (i == '\n') {
            /* End-of-line.  Time stamp is hex-encoded at start of line. */
            i++;
            index_append(i);
            (void)last;
            last = i;
        }
        else {
            /* Line character, just skip. */
            i++;
        }
    }
}

struct log_index {
    uintptr_t time;
    uintptr_t offset;
};

/* Note that it is surprisingly difficult to make a one-size-fits-all
   version of this without some extra information on how to handle
   non-exact matches (out of bounds, non-strictly increasing time
   stampes, lower/upper approximates...)  I'm going to let this sit
   until actual use gives more context. */

uintptr_t log_index_find(const struct log_index *i, uintptr_t min, uintptr_t max, uintptr_t time) {
    /* Handle degenerate cases. */
    if (min == max) return min;
    if (min > max) {
        uintptr_t tmp = min;
        min = max;
        max = tmp;
    }
    if (time <= i[min].time) return min;
    if (time >= i[max].time) return max;

    for(;;) {
        /* loop invariant: t_min < t < t_max */
        intptr_t half = (max - min) >> 1;
        if (unlikely(half == 0)) {
            /* They are adjacent so we can no longer split.  Which one
               to pick really depends on how this is used.  Currently
               we pick the largest one. */
            return max;
        }
        /* They are not adjacent, which means both intervals reduce,
           which means the loop will not get stuck.  Maintain the
           invariant. */
        uintptr_t mid = min + half;
        uintptr_t mid_time = i[mid].time;
        if (time < mid_time) {
            max = mid;
        }
        else if (mid_time < time) {
            min = mid;
        }
        else {
            /* Exact match. */
            return mid;
        }
    }
}


#endif
