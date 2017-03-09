/* To the extent possible under law, Tom Schouten has waived all
   copyright and related or neighboring rights to time_extend.h
   Code:    http://zwizwa.be/git/uc_tools
   License: http://creativecommons.org/publicdomain/zero/1.0 */


/* Allow for extending 16-bit timer capture values to 32-bit.

   This works around a race condition between performing the extension
   using an additional counter, and incrementing the counter on
   rollover.

   Instead, two extension counters are used, one for each region, and
   it is assumed the time_extend and time_extend_update() functions
   are called inside the proper phases.


   captured value range  | extension
   ----------------------+-------------
   0xC000 - 0xFFFF       | ext_wrap-1
   0x0000 - 0x4000       | ext_wrap

   0x4000 - 0xC000       | ext_mid


   at or around          | counter incremented
   -------------------------------------------
   0x0000                | ext_mid++
   0x8000                | ext_wrap++

*/

#ifndef TIME_EXTEND_H
#define TIME_EXTEND_H

#if 0
// There's a simpler approach, see below
struct time_extend {
    uint16_t ext_wrap;
    uint16_t ext_mid;
    uint16_t last_count;
};
#define TIME_EXTEND_Q1 (0x4000UL)
#define TIME_EXTEND_Q2 (0x8000UL)
#define TIME_EXTEND_Q3 (0xC000UL)
static inline void time_extend_update(struct time_extend *x, uint16_t current_count) {
    if ((x->last_count <  TIME_EXTEND_Q2) &&
        (current_count >= TIME_EXTEND_Q2)) {
        /* 0x8000 halfpoint was crossed */
        x->ext_wrap++;
    }
    else if ((x->last_count >= TIME_EXTEND_Q2) &&
             (current_count  < TIME_EXTEND_Q2)) {
        /* 0x0000 rollover was crossed */
        x->ext_mid++;
    }
    x->last_count = current_count;
}
static inline void time_extend_init(struct time_extend *x) {
    /* It is assumed this is called while count is still in the first
     * quarter. */
    x->ext_wrap = 0;
    x->ext_mid = 0;
    x->last_count = 0;
}
static inline uint32_t time_extend(struct time_extend *x, uint16_t value) {
    uint16_t ext = value;
    if (value < TIME_EXTEND_Q1) {
        ext = x->ext_wrap;
    }
    else if ((value >= TIME_EXTEND_Q1) &&
             (value <  TIME_EXTEND_Q3)) {
        ext = x->ext_mid;
    }
    else { // Q4
        ext = x->ext_wrap-1;
    }
    return value | (ext << 16);
}

#else

/* Simpler implementation, using the time ordering of events to
   disambiguate. */
struct time_extend {
    uint16_t time_lo;
    uint16_t time_hi;
};

/* Preconditions:
   T(V) means physical time of event that produced timer value in variable (V)
   P is physical timer period

   - T(time) - P < T(capture) <= T(time)
   - T(time_n+1) - T(time_n) < P

   Routine is idempotent and can be used to annotate multiple events. */
uint32_t time_extend(struct time_extend *x,
                     uint16_t capture, uint32_t time) {
    /* Update extension state. */
    if (time < x->time_lo) { x->time_hi++; }
    x->time_lo = time;

    /* Extend capture based on capture <= time condition. */
    uint32_t ext = capture < time ? x->time_hi : x->time_hi - 1;
    return (ext << 16) | capture;
}
void time_extend_init(struct time_extend *x) {
    x->time_lo = 0;
    x->time_hi = 0;
}

#endif


#endif //TIME_EXTEND_H

