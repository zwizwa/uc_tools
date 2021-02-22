/* Timing routines using the machine cycle clounter. */
#ifndef CYCLE_COUNTER_H
#define CYCLE_COUNTER_H

#include "stdint.h"

/* Having code depend on absolute time introduces some linker
   complications, e.g.:

   - We want code to be fast for specific platform, e.g. STM32F1

   - We also want a generic version, e.g. to be able to emulate code
     that depends on a notion absolute time.

   - We probably no not want to run such code in a non-polling
     situation, e.g. a Linux application (other than an emulator).

   Currently this is solved by providing the counter implementation
   for specific machines, and resort to an externally defined C
   function in other cases.
*/

#ifdef STM32F1

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
#else

#define CYCLE_COUNTER_NO_INLINE
uint32_t cycle_counter(void);
void enable_cycle_counter(void);

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
/* Note that =< 0 might be better.  In simulation I found that future
   time +1 expires at future time +2.  It is probably ok to change
   this, but it needs a code review to see if there are no
   pathological cases.  In general it seems that actual offset in
   ticks is quite high so one less shouldn't break anything. */
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
//#include "infof.h"
static inline void cycle_counter_init_timeout(struct cycle_counter_interval *cci, uint32_t timeout) {
    cci->expire = cycle_counter_future_time(timeout);
    //infof("expire %x\n", cci->expire);
}
static inline int cycle_counter_wait_timeout(struct cycle_counter_interval *cci) {
    uint32_t now = cycle_counter(); // sample once. value is used twice
    cci->diff = cycle_counter_diff(cci->expire, now);
    if (cci->diff < 0) return 1; // resume due to time expire
    return 0; // wait condition false and timer not yet expired
}

/* This macro can be used in conjunction with sm.h, but doesn't pull
   in the header.  Return value < 0 means timeout.  Note that timeout
   is evaluated first to make sure that ->diff gets updated on each
   try. */
#define SM_WAIT_CC_TIMEOUT(s, interval, timeout, condition) ({          \
            cycle_counter_init_timeout(interval, timeout);              \
            SM_WAIT(s, cycle_counter_wait_timeout(interval) || (condition)); \
            (interval)->diff; })

/* Similar, but for SM_READ. */
#define SM_READ_CC_TIMEOUT(s, interval, timeout, var, chan) ({          \
            cycle_counter_init_timeout(interval, timeout);              \
            SM_WAIT(s, ((var=0),cycle_counter_wait_timeout(interval)) || (var = sm_read(chan))) ; \
            (interval)->diff; })

/* Basic sleep using just a timestamp state. */
#define SM_CC_SLEEP(_s, _timestamp, _diff_cycles) {                     \
        _timestamp = cycle_counter();                                   \
        SM_WAIT(_s, (cycle_counter() - _timestamp) > _diff_cycles);     \
    }


#endif
