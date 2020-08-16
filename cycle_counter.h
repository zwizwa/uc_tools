/* ARM cycle counter. */
#ifndef CYCLE_COUNTER_H
#define CYCLE_COUNTER_H

/* Use the ARM 32-bit cycle counter to do time stamping.  It's more
   convenient than having to work around 16bit counter limitations.
   The 16 bit timers could later be used as oscillators. */

#include <libopencm3/cm3/dwt.h>
#define DEMCR  MMIO32(0xE000EDFC)
#define LAR    MMIO32(0xE0001FB0)

// https://stackoverflow.com/questions/36378280/stm32-how-to-enable-dwt-cycle-counter
static inline void enable_cycle_counter(void) {
    DEMCR |= 0x01000000;    // enable trace
    LAR = 0xC5ACCE55;       // <-- added unlock access to DWT (ITM, etc.)registers
    DWT_CYCCNT = 0;         // clear DWT cycle counter
    DWT_CTRL |= 1;          // enable DWT cycle counter
}
static inline uint32_t cycle_counter(void) {
    return DWT_CYCCNT;
}

static inline int32_t cycle_counter_remaining(uint32_t expiration_time) {
    uint32_t cc_cur = cycle_counter();

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
static inline int32_t cycle_counter_expired(uint32_t expiration_time) {
    return cycle_counter_remaining(expiration_time) < 0;
}
static inline uint32_t cycle_counter_future_time(uint32_t udiff) {
    return cycle_counter() + udiff;
}

#define CYCLE_COUNTER_EXPIRED(state) \
    ({uint32_t _cc_cur = cycle_counter() ; (_cc_cur - (state)) > (time);   \
        (state) = _cc_cur)

/* Run once every <time> cycles.  Needs a uint32_t state var.  time==0
   means disable. */
#define CYCLE_COUNTER_PERIODIC(state, time)             \
    for(uint32_t _cc_cur = cycle_counter();             \
        ((time) > 0) && (_cc_cur - (state)) > (time);   \
        (state) = _cc_cur)

#endif
