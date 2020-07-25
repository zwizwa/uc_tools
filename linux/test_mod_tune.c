#include "gdb/mod_tune.c"

/* Test mod_tune by providing a CSP task that performs the
   measurement.  It is structured as an RPC pattern to be able to
   implement it over an asynchronous channel, e.g. to tie it to the
   hardware. */

/* MEASURE TASK */
struct measure_emu {
    struct csp_task task;
    struct csp_evt evt[1];
    void *next;
    int freq;
    int chan;
};
csp_status_t measure_emu_tick(struct measure_emu *s) {
    SM_RESUME(s);
    for(;;) { /* RPC maps frequency to frequency measurement .*/

        CSP_RCV(&s->task, s, s->chan, s->freq);
        //LOG("measure_emu: req\n");

        s->freq += 123; // FIXME: measurement emulation
        CSP_SND(&s->task, s, s->chan, s->freq);
        //LOG("measure_emu: resp\n");
    }
}
void measure_emu_init(struct measure_emu *s, int chan) {
    memset(s,0,sizeof(*s));
    s->chan = chan;
}
void test1(struct csp_scheduler *s) {
    struct measure_emu m;
    struct sm_tune t;
    int chan = 0;
    SM_CSP_START(s, &m.task,     measure_emu, &m, chan);
    SM_CSP_START(s, &t.env.task, tune, &t, chan);
    csp_schedule(s);
}
void log_double_point(struct double_point *p) {
    LOG("(%0.2f,%0.2f} ", p->x, p->y);
}
void log_double(struct double_points *s) {
    log_double_point(&s->a);
    log_double_point(&s->b);
    log_double_point(&s->c);
    LOG("\n");
}
void log_fxp_point(struct fxp_point *p) {
    // LOG("(%f(%d),%f(%d)} ", DOUBLE(p->x), p->x, DOUBLE(p->y), p->y);
    LOG("(%0.2f,%0.2f} ", DOUBLE(p->x), DOUBLE(p->y));
}
void log_fxp(struct fxp_points *s) {
    log_fxp_point(&s->a);
    log_fxp_point(&s->b);
    log_fxp_point(&s->c);
    LOG("\n");
}

int main(int argc, char **argv) {
    /* First test the pure parts: linear interpolation. */
    if (1) {
        struct double_points dbl = {
            {0.1, 0.3},
            {0.2, 0.4},
            {0,   0.35}
        };
        double_solve_linear(&dbl);
        log_double(&dbl);

        struct fxp_points fxp = {
            {FIXED(0.1), FIXED(0.3)},
            {FIXED(0.2), FIXED(0.4)},
            {0,          FIXED(0.35)}
        };
        fxp_solve_linear(&fxp);
        log_fxp(&fxp);

    }

    /* Then run the dynamic code. */
    if (1) {
        int nb_c2e = 3;
        int nb_c = 3;
        csp_with_scheduler(nb_c2e, nb_c, test1);
    }
    return 0;
}
