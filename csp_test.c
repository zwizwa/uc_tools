#define CSP_HAVE_BUFFERED_SEND

#include "csp.c"

/* Some context
   ------------

   This started out as a sandbox for figuring out how to get on the
   async/await bandwagon, implementing CSP targeting plain C with only
   static allocation.

   During exploration it became clear that the idea of communicating
   processes (scheduler, select, channels) is independent of how those
   processes are implemented.

   A CSP scheduler only needs a resume method on a task, and a way to
   obtain the next blocking select action after resume.

   In practice I've identified these task structures:

   - Just dispatch: the resume function is a single select statement,
     constructed based on the current state.

   - Computed Goto (CG).  This allows a "flat" sequential program
     (only blocking calls in the main function) be expressed in fairly
     standard C.  This is fairly powerful, as CG can be interleaved
     with C control structures.

   - A "nested" sequential program, where blocking calls can happen
     inside of subroutine calls.  This can not easily be expressed in
     standard C, and would requires some syntactic frontend that can
     compile a program to e.g. CG form.  ( This was my original
     intention, but has been postponed until later as it is not a
     trivial task. Notes regarding CG as a compilation target are in
     sm.txt )
*/


/* Send a sequence of numbers. */
struct env0 {
    struct csp_task task;
    struct csp_evt evt[1];
    void *next;
    uint32_t x;
};
void task0(struct env0 *e) {
    if (e->next) goto *e->next;
    e->x = 0;
  again:
    LOG("task0: CSP_SND %d\n", e->x);
    CSP_SND(e, 1, e->x);
    e->x += 1;
    goto again;
}

/* "double" the sequence: for input n, send out 2n,2n+1 */
struct env1 {
    struct csp_task task;
    struct csp_evt evt[1];
    void *next;
    uint32_t a;
    uint32_t b;
};
void task1(struct env1 *e) {
    if (e->next) goto *e->next;
  again:
    CSP_RCV(e, 1, e->a);
    LOG("task1: CSP_RCV %d\n", e->a);

    e->b = e->a*2;
    LOG("task1: CSP_SND %d\n", e->b);
    CSP_SND(e, 2, e->b);

    e->b = e->a*2+1;
    LOG("task1: CSP_SND %d\n", e->b);
    CSP_SND(e, 2, e->b);
    goto again;
}


/* print 10 numbers and halt. */
struct env2 {
    struct csp_task task;
    struct csp_evt evt[1];
    void *next;
    uint32_t n;
    uint32_t x;
};
void task2(struct env2 *e) {
    if (e->next) goto *e->next;
    for(e->n = 0; e->n < 10; e->n++) {
        CSP_RCV(e, 2, e->x);
        LOG("task2: CSP_RCV %d\n", e->x);
    }
    e->task.resume = 0; // HALT
}




/* Run until task3 halts and everything blocks on CSP_SND. */
void test1(void) {
    struct env0 env0 = { .task = { .resume = (csp_resume_f)task0 } };
    struct env1 env1 = { .task = { .resume = (csp_resume_f)task1 } };
    struct env2 env2 = { .task = { .resume = (csp_resume_f)task2 } };
    struct csp_scheduler s = { };
    csp_start(&s, &env0.task);
    csp_start(&s, &env1.task);
    csp_start(&s, &env2.task);
    csp_schedule(&s);
}
/* Add something on a channel from the outside.  I.e. use the CSP
   network as a push reactive system.  Note that this requires a
   blocking receiving task to receive the value. */
void test2(void) {
    struct env1 env1 = { .task = { .resume = (csp_resume_f)task1 } };
    struct env2 env2 = { .task = { .resume = (csp_resume_f)task2 } };
    struct csp_scheduler s = { };
    csp_start(&s, &env1.task);
    csp_start(&s, &env2.task);
    uint32_t inp = 0;
    for (int i=0; i<3; i++) {
        /* Sucessful send.  Channel 1 has reader. */
        LOG("send: %d\n", csp_send(&s, 1, &inp, sizeof(inp)));
        inp++;
        /* Unsucessful send.  Channel 3 has no reader. */
        LOG("send: %d\n", csp_send(&s, 3, &inp, sizeof(inp)));
    }
}
/* Test the buffered external input. */
void test3(void) {
    struct env2 env2 = { .task = { .resume = (csp_resume_f)task2 } };
    struct csp_scheduler s = { };
    csp_start(&s, &env2.task);
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



