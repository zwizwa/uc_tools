/* Wrapper for cycle_counter based bit bang RLE signal generator. */

/* Note that the 1-channel sg_rle.h playback code is different from
   the multi-channel format used in ns_rle_enc.h / ns_rle_dec.h which
   is inteded for RLE compression of logic streams. */

#include <stdint.h>

/* Encode: FIXME: Move to different file. */

/* This is fencepost error territory, so start with some examples.

   The format encodes run lengths of the same bit value and cannot
   represent 0 run length, so the initial conditions make the
   assumption there is an implicit transition at the beginning of
   data, and that there is at least 1 byte in the data.

   Some context:
   - data[-1] == 1  (not represented)
   - data[0]  == 0

   Before | is implicit, after is explicit data[0], data[1], ...
   After -> is rle output.  High bit set means transition.

   1 | 0       -> 0x80
   1 | 0 1     -> 0x80, 0x80
   1 | 0 0     -> 0x81
   1 | 0 1 0   -> 0x80, 0x80, 0x80
   1 | 0 0 1   -> 0x81, 0x80
   1 | 0 0 1 1 -> 0x81, 0x81

*/

typedef uint8_t bus_t;

#include "macros.h"

struct rle_encode_state {
    uint32_t count;
    uint32_t o;
    uint8_t *rle_out;
};
static inline void rle_encode_write(struct rle_encode_state *s) {
    // LOG("count = %d\n", s->count);
    uintptr_t extra = s->count >> 7;
    s->count &= 0x7f;
    s->rle_out[s->o++] = s->count | 0x80;
    if (extra > 0) {
        s->rle_out[s->o++] = extra-1;
    }
    s->count = 0;
}

uintptr_t rle_encode(const bus_t *data, uintptr_t data_len,
                     uint8_t *rle_out, uintptr_t rle_out_len) {
    struct rle_encode_state s = {
        .count = 0,
        .o = 0,
        .rle_out = rle_out,
    };
    if (data_len < 1) return 0;
    uint32_t channel = 0;
    bus_t    state = 1 & (data[0] >> channel);
    for (int i=1; i<data_len; i++) {
        uint8_t bit = 1 & (data[i] >> channel);
        if (bit != state) {
            rle_encode_write(&s);
            state = bit;
        }
        else {
            s.count++;
        }
    }
    rle_encode_write(&s);
    return s.o;
}





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

uint8_t output[1000];

int cycle_counter_expired(uint32_t next_t) {
    // LOG("expired? %d %d,%d\n", next_t, lo, hi);
    if (hi) val = 1;
    if (lo) val = 0;
    // LOG("%d %d\n", t, val);
    while(t < next_t) {
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

    uint8_t rle[100];

    /* First byte in output is the idle level.
       rle_encode assumes the first byte is a transition. */
    uintptr_t rle_len = rle_encode(output+1, t-1, rle, sizeof(rle));
    LOG("enc:");
    char c = ' ';
    for(int i=0; i<rle_len; i++) {
        LOG("%c0x%02x", c, rle[i]);
        c = ',';
    }
    LOG("\n");


}
#define TEST(...) {                             \
        LOG("\nin: " #__VA_ARGS__ "\n");            \
        uint8_t buf[] = { __VA_ARGS__ };        \
        test(buf, sizeof(buf));                 \
    }

int main(int argc, char **argv) {
    /* Some 1 and 2 width pulses that are easy to verify by hand. */
    TEST(0x80);
    TEST(0x80,0x80);
    TEST(0x81);
    TEST(0x81,0x80);
    TEST(0x81,0x81);
    TEST(0x80,0x80,0x80,0x80);
    TEST(0x83,0x83);

    TEST(0xFF);

    TEST(0x80,0x00);
    TEST(0x80,0x01);

    /* Encode, decode automatic */
    return 0;
}
