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



/* Run once every <time> cycles.  Needs a uint32_t state var. */
#define CYCLE_COUNTER_PERIODIC(state, time) \
    for(uint32_t _cc_cur = cycle_counter(); \
        (_cc_cur - (state)) > (time); \
        (state) = _cc_cur)

#endif
