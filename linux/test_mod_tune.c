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
        LOG("measure_emu: req\n");

        s->freq += 123; // FIXME: measurement emulation
        CSP_SND(&s->task, s, s->chan, s->freq);
        LOG("measure_emu: resp\n");
    }
}
void measure_emu_init(struct measure_emu *s, int chan) {
    memset(s,0,sizeof(*s));
    s->chan = chan;
}
void test1(struct csp_scheduler *s) {
    struct measure_emu m;
    struct sm_tune t;
    int chan = 1;
    SM_CSP_START(s, &m.task,     measure_emu, &m, chan);
    SM_CSP_START(s, &t.env.task, tune, &t, chan);
    csp_schedule(s);
    LOG("\n");
}
int main(int argc, char **argv) {
   /* Sizes don't matter much for tests.  Just make sure they are
       large enough.  FIXME: create some functionality to compute
       storage parameters from application. */
    int nb_c2e = 20;
    int nb_c = 20;
    csp_with_scheduler(nb_c2e, nb_c, test1);
    return 0;
}
