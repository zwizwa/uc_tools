#include "xorshift.h"
#include "lcg.h"
#include "macros.h"
#include <inttypes.h>

uint64_t per_xorshift(void) {
    /* If it's full period, state can be anything except 0. */
    uint32_t seed = 1;
    uint32_t state = seed;
    uint64_t per = 0;
    for(;;) {
        srandom_u32(&state);
        per++;
        if (state == seed) {
            LOG("xorshift 0x%" PRIx64 "\n", per);
            return per;
        }
    }
}

uint64_t per_lcgn(uint32_t n) {
    uint32_t seed = 1;
    uint32_t state = seed;
    uint64_t per = 0;
    for(;;) {
        /* Coefficients from Numerical Recipes for n==2 */
        lcgn_next(&state, n);
        per++;
        // LOG(" %02x", state);
        if (state == seed) {
            LOG("lcgn %d 0x%" PRIx64 "\n", n, per);
            return per;
        }
    }
}

int main(int argc, char **argv) {

    /* Coefficients have been modified from popular defaults, so test
       that the period is still good. */

    // ASSERT(per_xorshift(seed) == 0xFFFFFFFFULL);  // 2^N - 1

    // LCG is 2^N
    for (uint32_t n=1; n<=32; n++) {
        ASSERT(per_lcgn(n) == (1ULL << n));
    }


    return 0;
}
