/* To the extent possible under law, Tom Schouten has waived all
   copyright and related or neighboring rights to counter.h
   Code:    http://zwizwa.be/git/uc_tools
   License: http://creativecommons.org/publicdomain/zero/1.0 */

#ifndef COUNTER_H
#define COUNTER_H

#include <stdint.h>

/* Chained circular counters.

   Constraints / remarks:
   - Implement non-modulo 2 chained counters.
   - Trivially guard against overflow (just add another overflow counter)
   - Atomic modulo compare (mainloop vs. ISR)
*/
struct counter;
struct counter {
    uint32_t count;
    uint32_t period;
    struct counter *carry;
};
INLINE void counter_init(struct counter *c) {
  next:
    c->count = 0;
    if ((c = c->carry)) goto next;
}
INLINE void counter_tick(struct counter *c) {
  next:
    c->count++;
    if (c->count >= c->period) {
        c->count = 0;
        if ((c = c->carry)) goto next;
    }
}
INLINE uint32_t counter_prev(struct counter *c) {
    return (c->count + c->period - 1) % c->period;
}

/* Convert chained circular counts to linear count.

   This is important for lock-free ISR operation, based on atomicity
   of 32bit load/store.

   Counters are used to index circular buffers *and* keep track of an
   overflow count.  To compare two counters irregardless of the
   overflow counter, include only the linear levels.

   To include overflow, use more levels but make sure the count still
   fits in 2^32.

*/
INLINE uint32_t counter_linear(struct counter *c, uint32_t levels) {
    uint32_t acc = 0;
    uint32_t period = 1;

    while(levels--) {
        acc += period * c->count;
        period *= c->period;
        if (!(c = c->carry)) break;
    }
    return acc;
}

INLINE void counter_round_up(struct counter *c) {
    if (c->count) {
        c->count = 0;
        counter_tick(c->carry);
    }
}

#endif // COUNTER_H

