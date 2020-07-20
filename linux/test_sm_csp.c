/* Test the interaction of sm.h based state machines and the csp
   scheduler.  This is done using two tasks:

   a counter, implemented as a single loop

   a processor, implemented as a loop making two calls to a
   submachine, with the submachie performing the csp blocking read.

   A change was made to separate the idea of CSP task and
   continuation.  The csp scheduler does not care about continuations
   of the individual tasks, but the select macro does rely on computed
   goto to set the next pointer.  This has been separated by providing
   two arguments: abstract continuation (the thing that has the next
   pointer) and concrete task struct.

   To allow sub-tasks to perform CSP functions, they need a pointer to
   the task struct.

*/

#include "macros.h"
#include "sm_csp.h"

/* COUNT TASK */
struct count {
    struct csp_task task;
    /* Task ends in a 0-length event array for which we provide
     * storage here. */
    struct csp_evt evt[1];
    /* It is our responsibility to represent the continuation. */
    void *next;
    int count;
    int chan;
};
csp_status_t count_tick(struct count *s) {
    SM_RESUME(s);
    for(;;) {
        LOG("count: %d\n", s->count);
        CSP_SND(&(s->task), s, s->chan, s->count);
        s->count++;
    }
}
void count_init(struct count *s, int chan) {
    memset(s,0,sizeof(*s));
    s->chan = chan;
}

/* PROC TASK + SUB*/
struct procsub {
    // struct csp_task task;
    /* Task ends in a 0-length event array for which we provide
     * storage here. */
    // struct csp_evt evt[1];
    /* It is our responsibility to represent the continuation. */
    void *next;
    int chan;
    int loops;
    int count;

    /* If the subtask wants to wait on events, it needs a reference to
       the task structure.  Use a wrapper to keep the shape of the
       macros.  FIXME: chean up that interface. */
    struct csp_task *task;
};
csp_status_t procsub_tick(struct procsub *s) {
    SM_RESUME(s);
    for(;;) {
        CSP_RCV(s->task, s, s->chan, s->count);
        LOG("procsub: %d %d\n", s->count, s->loops);
        if (!s->loops--) return SM_HALTED;
    }
}
void procsub_init(struct procsub *s, struct csp_task *task, int loops) {
    memset(s,0,sizeof(*s));
    s->loops = loops;
    s->task = task;
}

struct proc {
    struct csp_task task;
    /* Task ends in a 0-length event array for which we provide
     * storage here. */
    struct csp_evt evt[1];
    /* It is our responsibility to represent the continuation. */
    void *next;
    int loops;
    int chan;
    union {
        struct procsub procsub;
    } sub;
};
csp_status_t proc_tick(struct proc *s) {
    SM_RESUME(s);
    while(s->loops--) {
        SM_SUB(s, procsub, &s->task, 3);
        SM_SUB(s, procsub, &s->task, 2);
    }
  halt:
    SM_HALT(s);
}
void proc_init(struct proc *s, int chan) {
    memset(s,0,sizeof(*s));
    s->task.resume = (csp_resume_f)proc_tick;
    s->chan = chan;
    s->loops = 4;
}


/* SETUP */
void test1(struct csp_scheduler *s) {
    struct count c;
    struct proc p;
    SM_CSP_START(s, count, &c, 0 /* chan */);
    SM_CSP_START(s, proc, &p, 0 /* chan */);
    csp_schedule(s);
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
