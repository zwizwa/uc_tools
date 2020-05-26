#ifndef FIXEDPOINT_H
#define FIXEDPOINT_H

/* CM3 has no floating point, so use 32x32->64 UMULL instruction for
   implementing filters.  According to this:
   https://stackoverflow.com/questions/57755041/how-to-do-3232-bit-multiplication-in-cortex-m3
   the compiler can optimize ordinary to umull.  This seems to be the
   case.  Below asm was compiled with KEEP to isolate it.

 08003eb8 <fixedpoint_mul>:
 8003eb8:	fba0 2301 	umull	r2, r3, r0, r1
 8003ebc:	4618      	mov	r0, r3
 8003ebe:	4770      	bx	lr

   So this left here as an example.  In practice it is easier to use
   ordinary multiplication and explicit shifts, as that also allows

*/
static inline uint32_t fixedpoint_mul_shift(uint32_t a, uint32_t b, uint32_t n) {
    uint64_t ab = ((uint64_t)a) * ((uint64_t)b);
    return ab >> n;
}
static inline uint32_t fixedpoint_mul(uint32_t a, uint32_t b) {
    return fixedpoint_mul_shift(a,b,32);
}




/* This is a non-variable fixed point implementation derived from
   floating point implementation in creb project,
   modules++/DSPIfilters.h

   These are state variable filters.

*/

/* FIXME: There is also the multiply-accumulate UMLAL. */


struct fixedpoint_svf {
    // state data
    uint32_t d1;
    uint32_t d2;

    // pole data
    uint32_t ai;
    uint32_t ar;

    // zero data
    uint32_t c0;
    uint32_t c1;
    uint32_t c2;
};

/* Floating point code, adapted from creb with only renames:
      t_float d1t = ar * d1 + ai * d2 + input;
      t_float d2t = ar * d2 - ai * d1;
      output = c0 * input + c1 * d1 + c2 * d2;
      d1 = d1t;
      d2 = d2t; 


   ROADMAP:

   - First implement signed factional multiply accumulate

   - Then compute some actual coefficients to see if they fit in the
     factional scheme.

   - Check dynamic range

*/

/* Compiler uses SMLAL as long as inputs are casted from i32 to i64 */
#define I64(x) ((int64_t)(int32_t)(x))

static inline int32_t fixedpoint_svf_update(struct fixedpoint_svf *s, int32_t input) {
    uint64_t d1_next =
        I64(s->ar) * I64(s->d1) +
        I64(s->ai) * I64(s->d2);
    uint64_t d2_next =
        I64(s->ar) * I64(s->d2) +
        I64(s->ai) - I64(s->d1);
    uint64_t output =
        I64(s->c0) * I64(input) +
        I64(s->c1) * I64(s->d1) +
        I64(s->c2) * I64(s->d2);
    s->d1 = (d1_next >> 32) + input;
    s->d2 = (d2_next >> 32);
    return (output >> 32);
}

/* Assembly output
08003ecc <ortho_test>:
 8003ecc:	4913      	ldr	r1, [pc, #76]	; (8003f1c <ortho_test+0x50>)
 8003ece:	e92d 4ff0 	stmdb	sp!, {r4, r5, r6, r7, r8, r9, sl, fp, lr}
 8003ed2:	f8d1 c004 	ldr.w	ip, [r1, #4]
 8003ed6:	688c      	ldr	r4, [r1, #8]
 8003ed8:	f8d1 8000 	ldr.w	r8, [r1]
 8003edc:	fb84 670c 	smull	r6, r7, r4, ip
 8003ee0:	68cb      	ldr	r3, [r1, #12]
 8003ee2:	17e5      	asrs	r5, r4, #31
 8003ee4:	fbc8 6703 	smlal	r6, r7, r8, r3
 8003ee8:	68ce      	ldr	r6, [r1, #12]
 8003eea:	f8d1 9014 	ldr.w	r9, [r1, #20]
 8003eee:	fbcc 4506 	smlal	r4, r5, ip, r6
 8003ef2:	463a      	mov	r2, r7
 8003ef4:	ebb4 0608 	subs.w	r6, r4, r8
 8003ef8:	eb65 77e8 	sbc.w	r7, r5, r8, asr #31
 8003efc:	fb89 8908 	smull	r8, r9, r9, r8
 8003f00:	f8d1 a010 	ldr.w	sl, [r1, #16]
 8003f04:	f8d1 b018 	ldr.w	fp, [r1, #24]
 8003f08:	fbc0 890a 	smlal	r8, r9, r0, sl
 8003f0c:	fbcc 890b 	smlal	r8, r9, ip, fp
 8003f10:	4402      	add	r2, r0
 8003f12:	600a      	str	r2, [r1, #0]
 8003f14:	604f      	str	r7, [r1, #4]
 8003f16:	4648      	mov	r0, r9
 8003f18:	e8bd 8ff0 	ldmia.w	sp!, {r4, r5, r6, r7, r8, r9, sl, fp, pc}
 8003f1c:	20002000 	.word	0x20002000
*/


#endif
