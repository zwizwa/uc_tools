// see pdm.erl

// All in fixed point
#define ONE ((double)(0x100000000)
// NOTE is log FREQ
#define NOTE(x) ((uint32_t)(x * ONE))
#define FREQ(x) (x)

struct sm_measure {
    void *next;
}
#define SM_MEASURE(s, ...) \
    SM_CALL(&s->measure, __VA_ARGS__)

struct sm_tune {
    void *next;
    uint32_t inits[2], freq, nb_iter, nb_octaves, logmax, iter, octave;
    struct sm_measure measure;
};

void tune_init(struct *s) {
    s->inits[0] = NOTE(0.49);
    s->inits[1] = NOTE(0.51);
    s->freq = FREQ(55);
    s->nb_iter = 4;
    s->nb_octaves = 6;
    s->logmax = 24;
}
// Basic structure of machine is to perform two measurements to
// initialize, and then perform measurements inside the inner loop
// over octaves and root finding approx.
void tune_tick(struct sm_tune *s) {
    // Initial points are meausred once and reused to start each octave scan.
    // FIXME: initial measurement
    SM_MEASURE(s, s->inits[0]);
    SM_MEASURE(s, s->inits[1]);
    for (s->octave = 0; s->octave < s->nb_octaves; s->octave++) {
        for(s->iter = 0; s->iter < s->nb_iter; s->iter++) {
            // XC = XA + (YT-YA) * ((XB-XA) / (YB-YA)),
                XYC = Measure(XC),
            SM_MEASURE();
        }
        s->freq *= 2;
    }
}
