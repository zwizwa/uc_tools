#include "csp.c"

/* Some context
   ------------

   This started out as a sandbox for figuring out how to get on the
   async/await bandwagon, implementing CSP with tasks compiled to
   state machines, but targeting plain C with only static allocation.

   During exploration it became clear that the idea of communicating
   processes (scheduler, select, channels) is independent of how those
   processes are implemented.

   A CSP scheduler only needs a resume method on a task, and a way to
   obtain the next blocking select action after resume.

   In practice I've identified these 3 classes of task structures,
   each a little more complext than the former:

   - Just dispatch: the resume function is a single select statement,
     constructed based on some ad hoc state machine representation.
     E.g. an RPC server with two states (recv and send).

   - Computed Goto (CG).  This allows a "flat" sequential program
     (only blocking calls in the main function) to be expressed in
     fairly standard C.  This is already fairly powerful, as CG can be
     interleaved with C control structures, but can't use function
     composition.  Often CG is more convenient than an ad-hoc state
     machine.

   - A "nested" sequential program, where blocking calls can happen
     inside of subroutine calls.  This can not easily be expressed in
     standard C, and would require some syntactic frontend that can
     compile a program to e.g. CG form.

.  The latter was my original intention, but has been postponed until
   later as it is not a trivial task.  Notes regarding CG as a
   compilation target are in sm.txt

   Surprsing to me, it turns out the bare bones CG approach is already
   quite useful in practice.

*/


/* To use the CSP_SND / CSP_RCV macros from csp.h, a task needs to
   have a csp_task 'task' member, immediately followed by a csp_evt
   array.  That array need one slot for each event to wait on for the
   largest select operation.  */

/* Send a sequence of numbers. */
struct state0 {
    struct csp_task task;
    struct csp_evt evt[1];
    void *next;
    uint32_t x;
};
void resume0(struct state0 *e) {
    if (e->next) goto *e->next;
    e->x = 0;
  again:
    LOG("0: CSP_SND %d\n", e->x);
    CSP_SND(e, 1, e->x);
    e->x += 1;
    goto again;
}

/* "double" the sequence: for input n, send out 2n,2n+1 */
struct state1 {
    struct csp_task task;
    struct csp_evt evt[1];
    void *next;
    uint32_t a;
    uint32_t b;
};
void resume1(struct state1 *e) {
    if (e->next) goto *e->next;
  again:
    CSP_RCV(e, 1, e->a);
    LOG("1: CSP_RCV %d\n", e->a);

    e->b = e->a*2;
    LOG("1: CSP_SND %d\n", e->b);
    CSP_SND(e, 2, e->b);

    e->b = e->a*2+1;
    LOG("1: CSP_SND %d\n", e->b);
    CSP_SND(e, 2, e->b);
    goto again;
}


/* print 10 numbers and halt. */
struct state2 {
    struct csp_task task;
    struct csp_evt evt[1];
    void *next;
    uint32_t n;
    uint32_t x;
};
void resume2(struct state2 *e) {
    if (e->next) goto *e->next;
    for(e->n = 0; e->n < 10; e->n++) {
        CSP_RCV(e, 2, e->x);
        LOG("2: CSP_RCV %d\n", e->x);
    }
    e->task.resume = 0; // HALT
}




/* Run until task3 halts and everything blocks on CSP_SND. */
void test1(void) {
    struct state0 state0 = { .task = { .resume = (csp_resume_f)resume0 } };
    struct state1 state1 = { .task = { .resume = (csp_resume_f)resume1 } };
    struct state2 state2 = { .task = { .resume = (csp_resume_f)resume2 } };
    struct csp_scheduler s = { };
    csp_start(&s, &state0.task);
    csp_start(&s, &state1.task);
    csp_start(&s, &state2.task);
    csp_schedule(&s);
}
/* Add something on a channel from the outside.  I.e. use the CSP
   network as a push reactive system.  Note that this requires a
   blocking receiving task to receive the value. */
void test2(void) {
    struct state1 state1 = { .task = { .resume = (csp_resume_f)resume1 } };
    struct state2 state2 = { .task = { .resume = (csp_resume_f)resume2 } };
    struct csp_scheduler s = { };
    csp_start(&s, &state1.task);
    csp_start(&s, &state2.task);
    uint32_t inp = 0;
    for (int i=0; i<3; i++) {
        /* Sucessful send.  Channel 1 has reader. */
        ASSERT(1 == csp_send(&s, 1, &inp, sizeof(inp)));
        /* Unsucessful send.  Channel 3 has no reader. */
        ASSERT(0 == csp_send(&s, 3, &inp, sizeof(inp)));
        inp++;
    }
}
/* Test the buffered external input. */
void test3(void) {
    struct state2 state2 = { .task = { .resume = (csp_resume_f)resume2 } };
    struct csp_scheduler s = { };
    csp_start(&s, &state2.task);
    uint8_t buf[64];
    struct csp_cbuf b;
    int ch_int = 1, ch_data = 2;
    csp_cbuf_start(&s, &b, ch_int, ch_data, buf, sizeof(buf));
    uint8_t msg[] = {1,2,3};
    csp_cbuf_write(&s, &b, &msg, sizeof(msg));
    csp_cbuf_notify(&s, &b, 1);
    csp_cbuf_write(&s, &b, &msg, sizeof(msg));
    csp_cbuf_notify(&s, &b, 1);
}

#ifndef ARRAY_SIZE
#define ARRAY_SIZE(x) (sizeof(x)/sizeof(x[0]))
#endif

/* Indexed scheduler test. */
void test4(void) {
    struct csp_ischeduler s;
    struct csp_evt_list c2e[100];
    struct csp_chan_to_evt c[100];
    csp_ischeduler_init(&s, c2e, ARRAY_SIZE(c2e), c, ARRAY_SIZE(c));

    struct state0 state0 = { .task = { .resume = (csp_resume_f)resume0 } };
    struct state1 state1 = { .task = { .resume = (csp_resume_f)resume1 } };
    struct state2 state2 = { .task = { .resume = (csp_resume_f)resume2 } };
    struct csp_scheduler s = { };
    csp_start(&s, &state0.task);
    csp_start(&s, &state1.task);
    csp_start(&s, &state2.task);
    csp_schedule(&s);
}
