#ifndef XORSHIFT_H
#define XORSHIFT_H

#include <stdint.h>

// Xorshift RNG
// https://www.jstatsoft.org/article/view/v008i14
// https://en.wikipedia.org/wiki/Xorshift
static inline uint32_t srandom_u32(uint32_t *x) {
    *x ^= (*x << 13);
    *x ^= (*x >> 17);
    *x ^= (*x << 5);
    return *x;
}

// It's not a good idea to put this in a .h file.  Should go into mod_*.c
// So make it optional.
#ifndef XORSHIFT_STATIC
#define XORSHIFT_STATIC 0
#endif

#if XORSHIFT_STATIC
static inline uint32_t random_u32(void) {
    static uint32_t x = 2463534242;
    return srandom_u32(&x);
}
#endif

#endif
