#ifndef XORSHIFT_H
#define XORSHIFT_H

#include <stdint.h>

// Xorshift RNG
// https://www.jstatsoft.org/article/view/v008i14
uint32_t srandom_u32(uint32_t *x) {
    *x ^= (*x << 13);
    *x ^= (*x >> 17);
    *x ^= (*x << 5);
    return *x;
}
uint32_t random_u32(void) {
    static uint32_t x = 2463534242;
    return srandom_u32(&x);
}

#endif
