#ifndef LCG_H
#define LCG_H

#include <stdint.h>

/* LCGs are period 2^N which is useful for permutations of data blocks. */
static inline uint32_t lcgn_generic_next(
    uint32_t *x, uint32_t a, uint32_t c, uint64_t bits) {
    uint32_t mask = (1ULL << bits)-1;
    *x = (a * (*x) + c) & mask;
    return *x;
}

/* Coefficients from Numerical Recipes for n==32 */
#define LCG_A 1664525
#define LCG_C 1013904223
/* These are period-tested in test_prng.c up to n==32 */
static inline uint32_t lcgn_next(uint32_t *x, uint64_t bits)  {
    return lcgn_generic_next(x, LCG_A, LCG_C, bits);
}


#endif
