
/* Tuning machine.

   I'm using this module as a driver to test the Linux emulation code,
   and to also move away from condition polling, into event-driven
   CSP.  The SM and CSP abstractions can now work together.

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

#include "sm_csp.h"

#include <stdint.h>


// see pdm.erl

// All in fixed point
#define ONE ((double)0x100000000)
// NOTE is log FREQ
#define FIXED(x) ((uint32_t)(x * ONE))
#define FREQ(x) (x)

#define DOUBLE(x) (((double)(x))/ONE)

/* Sub machine uses a raw measurement RPC on a channel to perform a
   conditioned measurement.  This is to allow emulation and network
   transparency .*/

/* Trying out a pattern: main machine and sub machine share some data.
   Put that in an environment struct. */
struct sm_tune_env {
    struct csp_task task;
    struct csp_evt evt[1];
    uint32_t setpoint;
    uint32_t value;
    int chan;
};
struct sm_measure {
    void *next;
    struct sm_tune_env *env;
};
void measure_init(struct sm_measure *s, struct sm_tune_env *env) {
    memset(s,0,sizeof(*s));
    s->env = env;
}
uint32_t measure_tick(struct sm_measure *s) {
    struct sm_tune_env *e = s->env;
    SM_RESUME(s);

    //LOG("meas1\n");
    CSP_RPC(&e->task, s, e->chan, e->setpoint, e->value);
    // LOG("drop: %d\n", e->value);

    //LOG("meas1\n");
    CSP_RPC(&e->task, s, e->chan, e->setpoint, e->value);
    // LOG("keep: %d\n", e->value);

    SM_HALT(s);
}


/* Given two points and a y value, compute the corresponding x value. */
/* Floating point reference implementation to validate fixedpoint version. */
struct double_point { double x,y; };
struct double_points { struct double_point a,b,c; };
static inline void double_solve_linear(struct double_points *s) {
    double slope = (s->b.x - s->a.x) / (s->b.y - s->a.y);
    s->c.x = s->a.x + (s->c.y - s->a.y) * slope;
}

/* Signed or unsigned math?  TBD. */
struct fxp_point { uint32_t x,y; };
struct fxp_points { struct fxp_point a,b,c; };
static inline void fxp_solve_linear(struct fxp_points *s) {
    uint32_t slope = (s->b.x - s->a.x) / (s->b.y - s->a.y);
    s->c.x = s->a.x + (s->c.y - s->a.y) * slope;
}




/* Main machine uses performs multiple calls to measurement sub
   machine to create a lookup table. */
struct sm_tune {
    struct sm_tune_env env;
    void *next;
    struct fxp_points points;
    uint32_t freq, nb_iter, nb_octaves, logmax, iter, octave;
    union {
        /* One entry for each SM_SUB call. */
        struct sm_measure measure;
    } sub;
};

/* Local macro.  This is complicated due to using the env struct. */
#define MEASURE(_setpoint) \
    ({ s->env.setpoint = _setpoint; \
       SM_SUB(s, measure, &s->env); \
       s->env.value; })


/* Basic structure of machine is to perform two measurements to
   initialize, and then perform measurements inside the inner loop
   over octaves and root finding approx. */
uint32_t tune_tick(struct sm_tune *s) {
    SM_RESUME(s);
    // Initial points are meausred once and reused to start each octave scan.
    // FIXME: initial measurement
    s->points.a.y = MEASURE(s->points.a.x);
    s->points.b.y = MEASURE(s->points.a.y);
    for (s->octave = 0; s->octave < s->nb_octaves; s->octave++) {
        for(s->iter = 0; s->iter < s->nb_iter; s->iter++) {
            s->points.c.y = 0; // FIXME: computed from freq?, see rdm.erl
            fxp_solve_linear(&s->points);
            s->points.c.y = MEASURE(s->points.c.x);
        }
        s->freq *= 2;
    }
  halt:
    SM_HALT(s);
}

void tune_init(struct sm_tune *s, int chan) {
    memset(s,0,sizeof(*s));
    s->points.a.x = 'A'; //FIXED((0.49);
    s->points.b.x = 'X'; //FIXED((0.51);
    s->freq = FREQ(55);
    s->nb_iter = 4;
    s->nb_octaves = 6;
    s->logmax = 24;
    s->env.chan = chan;
}

#undef MEASURE
