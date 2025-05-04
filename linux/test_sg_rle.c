/* Test wrapper for cycle_counter based bit bang RLE signal generator
   and data encoder.

   Notes:

   - This is about the 1-channel ad-hoc sg_rle.h playback code that
     was originally designed to replay uart bus captures.

   - It has a limitation that it can only represent run lengths up to
     (* 129 128) 16512 samples or 16.512 ms at 1MHz sample rate, which
     works for the application domain.

   - This is all different from the multi-channel format used in
     ns_rle_enc.h / ns_rle_dec.h which is inteded for RLE compression
     of logic streams and has less limitations as it uses LEB128 run
     length encoding and explicit bus values.

   - This code simulates the signal generator, prints some manually
     verifiable output + runs the encoder on the output and verifies
     the round-trip is the same.

   - This code is written in a time where I am seriously overworked.
     It doesn't turn out to be the right approach actually, and I have
     to redo it again.


*/


/* Encoder */
#include "sg_rle_enc.h"


/* Player + stubs */

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

uint8_t output[1000000];

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


/* Test playback + re-encode + verify */
void test(uint8_t *rle_ops, uint32_t nb) {
    /* Reset state. */
    t = 0;
    val = 1;
    struct sg_rle_ctx ctx = {
        .tx = {&lo, &hi},
    };
    memset(output,0x55,sizeof(output));


    sg_rle_8(&ctx, rle_ops, nb);
    /* Run one more time to get the end state. */
    cycle_counter_expired(t+1);


    /* If it's a small signal print it out for visual check. */
    if (t < 500) {
        for (int i=0; i<t; i++) {
            LOG(" %d", output[i]);
        }
        LOG("\n");
    }
    else {
        LOG("...\n");
    }

    /* Limit size here is limitation set elsewhere. */
    uint8_t rle[516];

    /* First byte in output is the idle level.
       sg_rle_enc assumes the first byte is a transition. */
    uintptr_t rle_len = sg_rle_enc(output+1, t-1, rle, sizeof(rle));
    LOG("enc:");
    char c = ' ';
    for(int i=0; i<rle_len; i++) {
        LOG("%c0x%02x", c, rle[i]);
        c = ',';
    }
    LOG("\n");

    ASSERT(nb == rle_len);
    for (int i=0; i<rle_len; i++) {
        ASSERT(rle_ops[i] == rle[i]);
    }

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

    TEST(0x80,0x00,0x80);
    TEST(0x80,0x01,0x80);

    TEST(0x80,0x7F,0x80);
    TEST(0x80,0x7F,0x85,0x23,0x80);

    TEST(0x80,0x7F,0x85,0x83,0x23,0xA8,0x56,0x81);

    return 0;
}
