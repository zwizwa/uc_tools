

/* To the extent possible under law, Tom Schouten has waived all
   copyright and related or neighboring rights to time_extend.h
   Code:    http://zwizwa.be/git/uc_tools
   License: http://creativecommons.org/publicdomain/zero/1.0 */

#ifndef TIME_EXTEND_H
#define TIME_EXTEND_H

/* Extend timer capture values based on periodic timer reads and
   guaranteed event order. */
struct time_extend {
    uint16_t time_lo;
    uint16_t time_hi;
};

/* Preconditions:
   T(V) means physical time of event that produced timer value in variable (V)
   P is physical timer period
   time_n is time variable of time_extend() call number n

   - T(time) - P < T(capture) <= T(time)
   - T(time_{n+1}) - T(time_n) < P

   - call time_extend_set_upper_bound() before calling
     time_extend_captured_value() possibly multiple times for multiple
     capture values.

*/
static inline void time_extend_set_upper_bound(struct time_extend *x, uint32_t time) {
    /* Update extension state. */
    if (time < x->time_lo) { x->time_hi++; }
    x->time_lo = time;
}
static inline uint32_t time_extend_captured_value(struct time_extend *x, uint16_t capture) {
    /* Extend capture based on capture <= time condition. */
    uint32_t ext = capture <= x->time_lo ? x->time_hi : x->time_hi - 1;
    return (ext << 16) | capture;
}
static inline void time_extend_init(struct time_extend *x) {
    x->time_lo = 0;
    x->time_hi = 0;
}



#endif //TIME_EXTEND_H

