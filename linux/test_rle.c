/* Wrapper for cycle_counter based bit bang RLE signal generator. */

#include <stdint.h>
#include "macros.h"

/* Some stubs to make it compile in emulation context. */
#define HW_CPU_MHZ 1
void iwdg_reset(void) {}

/* Only used once to initialize. */
uint32_t t = 0;
uint32_t cycle_counter(void) { return 0; }

/* All the other calls are this, so we use it to advance time. */
uint32_t lo=0, hi=0;
uint32_t val = 1;

uint8_t output[100];

int cycle_counter_expired(uint32_t next_t) {
    // LOG("expired? %d %d,%d\n", next_t, lo, hi);
    if (hi) val = 1;
    if (lo) val = 0;
    while(t < next_t) {
        LOG("%d %d\n", t, val);
        ASSERT(t < sizeof(output));
        output[t++] = val;
    }
    lo=0;
    hi=0;
    t = next_t;
    return 1;
}

#include "sg_rle.h"

void test(uint8_t *rle_ops, uint32_t nb) {
    t = 0;
    val =1;
    struct sg_rle_ctx ctx = {
        .tx = {&lo, &hi},
    };
    sg_rle_8(&ctx, rle_ops, nb);
    /* Run one more time to get the end state. */
    cycle_counter_expired(t+1);
}
#define TEST(...) {                             \
        LOG("\n" #__VA_ARGS__ "\n");            \
        uint8_t buf[] = { __VA_ARGS__ };        \
        test(buf, sizeof(buf));                 \
    }

int main(int argc, char **argv) {
    TEST(0x80);
    TEST(0x80,0x80);
    TEST(0x81);
    TEST(0x81,0x80);
    TEST(0x81,0x81);
    return 0;
}
