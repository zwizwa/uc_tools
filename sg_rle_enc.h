#ifndef SG_RLE_ENC_H
#define SG_RLE_ENC_H

/* Encoder for sg_rle.h signal generator.

   This is fencepost error territory, so start with some examples.
   See also test_sg_rle.c

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


#include <stdint.h>

struct sg_rle_enc_state {
    uintptr_t count;
    uintptr_t o;
    uintptr_t rle_out_len;
    uint8_t *rle_out;
};
static inline void sg_rle_enc_write(struct sg_rle_enc_state *s) {
    // LOG("count = %d\n", s->count);
    uintptr_t extra = s->count >> 7;
    s->count &= 0x7f;
    if (s->o < s->rle_out_len) {
        s->rle_out[s->o++] = s->count | 0x80;
    }
    if (extra > 0) {
        if (s->o < s->rle_out_len) {
            s->rle_out[s->o++] = extra-1;
        }
    }
    s->count = 0;
}

typedef uint8_t sg_rle_bus_t;

uintptr_t sg_rle_enc(const sg_rle_bus_t *data, uintptr_t data_len,
                     uint8_t *rle_out, uintptr_t rle_out_len) {
    struct sg_rle_enc_state s = {
        .count = 0,
        .o = 0,
        .rle_out = rle_out,
        .rle_out_len = rle_out_len,
    };
    if (data_len < 1) return 0;
    uint32_t channel = 0;
    sg_rle_bus_t state = 1 & (data[0] >> channel);
    for (int i=1; i<data_len; i++) {
        uint8_t bit = 1 & (data[i] >> channel);
        if (bit != state) {
            sg_rle_enc_write(&s);
            state = bit;
        }
        else {
            s.count++;
        }
    }
    sg_rle_enc_write(&s);
    return s.o;
}


#endif

