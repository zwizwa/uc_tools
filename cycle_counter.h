/* ARM cycle counter. */
#ifndef CYCLE_COUNTER_H
#define CYCLE_COUNTER_H



/* Hardware-specific. */
#ifndef EMU

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
#endif



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



static inline int32_t cycle_counter_remaining(uint32_t expiration_time) {
    return cycle_counter_diff(expiration_time, cycle_counter());
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



/* Abstraction to use in conjuction with sm.h : SM_WAIT with timeout. */
struct cycle_counter_interval {
    uint32_t expire;  // absolute expiration time
    int32_t diff;     // relative remaining time, negative means timeout
};
static inline void cycle_counter_init_timeout(struct cycle_counter_interval *cci, uint32_t timeout) {
    cci->expire = cycle_counter_future_time(timeout);
}
static inline int cycle_counter_wait_timeout(struct cycle_counter_interval *cci, int wait_condition) {
    if (wait_condition) return 1; // resume due to wait condition true
    uint32_t now = cycle_counter(); // sample once. value is used twice
    cci->diff = cycle_counter_diff(cci->expire, now);
    if (cci->diff < 0) return 1; // resume due to time expire
    return 0; // wait condition false and timer not yet expired
}

/* This macro can be used in conjunction with sm.h, but doesn't pull
   in the header.  Return value < 0 means timeout. */
#define SM_WAIT_CC_TIMEOUT(s, interval, timeout, condition) ({             \
            cycle_counter_init_timeout(interval, timeout);              \
            SM_WAIT(s, cycle_counter_wait_timeout(interval, condition)); \
            (interval)->diff; })


#endif
