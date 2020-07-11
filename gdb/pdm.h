#ifndef PDM_H
#define PDM_H

#include <stdint.h>

/* First order multi-bit output noise shaper, e.g. to use in
   conjunction with hardware PWM or DAC.  Note that in a 1-bit first
   order PDM, it is possible to use the carry bit output of the last
   integrator.  We don't use that approach here. */
struct pdm1 {
    uint32_t s1;
};
static inline uint32_t pdm1_update(struct pdm1 *p, uint32_t input, uint32_t out_shift) {
    /* For a simpler output equation, the output is delayed one
       sample, so we can quantize the last state before computing the
       next update. */
    uint32_t out_q = p->s1 >> out_shift;

    /* The approximation error due to quantization drives the loop filter. */
    uint32_t error = input - (out_q << out_shift);
    p->s1 += error;

    return out_q;
} __attribute__((always_inline))

/* Similar for second and third order loop filters. */

struct pdm2 {
    uint32_t s1;
    uint32_t s2;
};
static inline uint32_t pdm2_update(struct pdm2 *p, uint32_t input, uint32_t out_shift, uint32_t dither) {
    uint32_t out_q = p->s2 >> out_shift;
    uint32_t out_a = (out_q << out_shift) + dither;

    p->s1 += input - out_a;
    p->s2 += p->s1 - out_a;

    return out_q;
} __attribute__((always_inline))

struct pdm3 {
    uint32_t s1;
    uint32_t s2;
    uint32_t s3;
};

static inline uint32_t pdm3_update(struct pdm3 *p, uint32_t input, uint32_t out_shift, uint32_t dither) {
    uint32_t out_q = p->s3 >> out_shift;
    uint32_t out_a = (out_q << out_shift) + dither;

    p->s1 += input - out_a;
    p->s2 += p->s1 - out_a;
    p->s3 += p->s2 - out_a;

    return out_q;
} __attribute__((always_inline))


struct pdm4 {
    uint32_t s1;
    uint32_t s2;
    uint32_t s3;
    uint32_t s4;
};

static inline uint32_t pdm4_update(struct pdm4 *p, uint32_t input, uint32_t out_shift, uint32_t dither) {
    uint32_t out_q = p->s4 >> out_shift;
    uint32_t out_a = (out_q << out_shift) + dither;

    p->s1 += input - out_a;
    p->s2 += p->s1 - out_a;
    p->s3 += p->s2 - out_a;
    p->s4 += p->s3 - out_a;

    return out_q;
} __attribute__((always_inline))



#endif
