
/* Tuning machine.

   I'm using this module as a driver to test the Linux emulation code,
   and to also move away from condition polling, into event-driven
   CSP.

   The basic idea is to be able to develop code on Linux and then move
   it to the microcontroller while changing as little as possible.

   Regarding CSP and Linux emulation, there are some things to note:

   - Splitting a CSP network across TCP links introduces asynchronous
     channels. This took a while to appreciate, but seems to be an
     significant change of semantics.

   - Asynchronous channels do not support "sync on send", but can
     still host a 2-step RPC mechanism.

   - This is only an issue for CSP, not for the sm.h mechanism.  It is
     not clear if I really want to push CSP forward, or if condition
     polling is good enough.
*/

#include "sm.h"

#include <stdint.h>


// see pdm.erl

// All in fixed point
#define ONE ((double)0x100000000)
// NOTE is log FREQ
#define NOTE(x) ((uint32_t)(x * ONE))
#define FREQ(x) (x)

struct sm_measure {
    void *next;
    uint32_t value;
};
void measure_init(struct sm_measure *s, int init) {
}
uint32_t measure_tick(struct sm_measure *s) {
    SM_RESUME(s);
    // set frequency
    // initiate a dummy measurement during transition and discard
    // initiate a (number of) good measurements
    // return result
    s->value = 123;
    SM_HALT(s);
}


struct sm_tune {
    void *next;
    uint32_t xa, xb, xc, ya, yb, yc;
    uint32_t freq, nb_iter, nb_octaves, logmax, iter, octave;
    union {
        /* One entry for each SM_SUB call. */
        struct sm_measure measure;
    } sub;
};

void tune_init(struct sm_tune *s) {
    s->xa = NOTE(0.49);
    s->xb = NOTE(0.51);
    s->freq = FREQ(55);
    s->nb_iter = 4;
    s->nb_octaves = 6;
    s->logmax = 24;
}
// Basic structure of machine is to perform two measurements to
// initialize, and then perform measurements inside the inner loop
// over octaves and root finding approx.
uint32_t tune_tick(struct sm_tune *s) {
    SM_RESUME(s);
    // Initial points are meausred once and reused to start each octave scan.
    // FIXME: initial measurement
    s->ya = SM_SUB(s, measure, s->xa)->value;
    s->yb = SM_SUB(s, measure, s->xb)->value;
    for (s->octave = 0; s->octave < s->nb_octaves; s->octave++) {
        for(s->iter = 0; s->iter < s->nb_iter; s->iter++) {
            uint32_t yt = 0; // FIXME: computed from freq?, see rdm.erl
            uint32_t slope = (s->xb - s->xa) / (s->yb - s->ya);
            s->xc = s->xa + (yt - s->ya) * slope;
            s->yc = SM_SUB(s, measure, s->xc)->value;
        }
        s->freq *= 2;
    }
  halt:
    SM_HALT(s);
}
