#include "stm32f103/pdm.h"
#include "macros.h"

#include <stdint.h>

/* Init to 0 is fine. */
struct pdm1 pdm1; uint32_t update1(uint32_t i, uint32_t s) { return pdm1_update(&pdm1, i, s); }
struct pdm2 pdm2; uint32_t update2(uint32_t i, uint32_t s) { return pdm2_update(&pdm2, i, s); }
struct pdm3 pdm3; uint32_t update3(uint32_t i, uint32_t s) { return pdm3_update(&pdm3, i, s); }

void test(uint32_t (*update)(uint32_t, uint32_t), uint32_t input, uint32_t shift, uint32_t n) {
    uint64_t acc = 0;
    for (int i=0; i<n; i++) {
        uint32_t out_q = update(input, shift);
        if (n <= 100) {
            LOG(" %d", out_q);
        }
        acc += out_q;
    }
    LOG("(%f)\n", ((float)acc) / ((float)(n*(1<<(32-shift)))));
}

int main(int argc, char **argv) {
    LOG("test_pdm.c\n");
    uint32_t i = 0x81234567;
    uint32_t shift = 24;
    uint32_t n = 1000;
    test(update1, i, shift, n);
    test(update2, i, shift, n);
    test(update3, i, shift, n);

    LOG("%f\n", ((float)i) / ((float)(1ULL << 32)));

}
