#ifndef CYCLE_COUNTER_GENERIC_H
#define CYCLE_COUNTER_GENERIC_H

/* Some generic routines to work with the ARM 32 bit cycle counter,
   without access to the counter iteself (see cycle_counter() in
   cycle_conter.h)

   This is mostly here to move towards a HAL where cycle_counter() is
   no longer a global entity, but accessed via a context pointer.

*/

/* Generic routines. */
static inline int32_t cycle_counter_diff(uint32_t expiration_time, uint32_t cc_cur) {
    /* Mixing unsigned and signed ints is tricky.  To make it more
       explicit, split the two operations that are used: */

    /* 1. Unsigned overflow is defined behavior in C.  It does what
       one would expect: wrap around.  */
    uint32_t udiff = expiration_time - cc_cur;

    /* 2. Converting unsigned to signed is defined as well. */
    int32_t idiff = udiff;

    /* So this number is negative if cc_cur has passed expiration time
       as long as it doesn't overflow.  For a 72MHz counter that is
       about half a minute. */
    return idiff;
}

/* Future time is calculated by adding a positive time offset */
static inline int32_t cycle_counter_future(uint32_t cc_cur, uint32_t udiff) {
    return cc_cur + udiff;
}


#endif
