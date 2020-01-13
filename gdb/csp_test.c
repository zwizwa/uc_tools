#define CSP_HAVE_BUFFERED_SEND

#include "csp.c"

/* Some context
   ------------

   This started out as a sandbox for figuring out how to compile from
   sequential form to state machine, as is done e.g. in the
   implementation of the promisise and async/await language extensions
   of C#, Rust, JavaScript etc..  It can be done in C with some
   restrictions on the kind of continuations that are use.  My
   original goal was to create something like CSP or Go, but stripped
   down and able to compile to malloc-less C for better integration in
   bare bones microcontroller code.

   During exploration it became clear that the idea of communicating
   processes is completely independent of how those processes are
   implemented, and that a CSP scheduler can also be built using just
   finite state machines.  This has been done in csp.c

   Compiling a task to a state machine is then left as a future
   problem to solve.  This file still contains some remarks but most
   design notes are in sm.txt (ask Tom).

   The program I started out with is below.  The language is a subset
   of C extended with CSP style channels.  test1 below is a manually
   compiled version of this.

   ;; k0
   (let ((a 123))
     (loop
       (let* ((b (recv)))  ;; k1
              (c (+ a b)))
         (send c)   ;; k2
         (send c))) ;; k3

   FIXME: Change the lisp to correspond to task1 again.  I've changed
   it to make the traces easier to read.

*/



#define SND CSP_SEND
#define RCV CSP_RECV

/* The context's type is not known to the scheduler.  Even the next
   instruction pointer can be kept abstract.  All the scheduler needs
   to know relates to the channel operation that needs to be
   performed, and the resume function that needs to be called after
   the operation completes.

   The context is allowed to be modified in-place.  It could be
   implemented as a C-like stack.

   A convenient way to implement state machines is computed goto (CG).
   This works for manually coded machines, but I've also settled on CG
   as a compilation target, as CG makes it possible to implement
   control structures and suspensions using the same continuation
   mechanism.  CG also is a bit more concise at the C level.

   The task1 example is a little too simple to test continuation
   compilation as it does not have a forking lexical context.  To
   explore that, add a conditional in subsequent iterations.  In
   test1, the "stack" only has one shape, and we implement different
   stages of completion by simply leaving part uninitialized.  */

struct env1 {
    struct csp_task task;
    struct csp_op op[1];
    void *next;
    uint32_t a;
    uint32_t b;
};
void task1(struct env1 *e) {
    if (e->next) goto *e->next;
  again:
    RCV(e, 1, e->a, k1);
    LOG("task1: RCV %d\n", e->a);

    e->b = e->a*2;
    LOG("task1: SND %d\n", e->b);
    SND(e, 2, e->b, k2);

    e->b = e->a*2+1;
    LOG("task1: SND %d\n", e->b);
    SND(e, 2, e->b, k3);
    goto again;
}


/* Two more tasks to test task1 */
struct env0 {
    struct csp_task task;
    struct csp_op op[1];
    void *next;
    uint32_t x;
};
void task0(struct env0 *e) {
    if (e->next) goto *e->next;
    e->x = 0;
  again:
    LOG("task0: SND %d\n", e->x);
    SND(e, 1, e->x, k1);
    e->x += 1;
    goto again;
}

struct env2 {
    struct csp_task task;
    struct csp_op op[1];
    void *next;
    uint32_t n;
    uint32_t x;
};
void task2(struct env2 *e) {
    if (e->next) goto *e->next;
    for(e->n = 0; e->n < 10; e->n++) {
        RCV(e, 2, e->x, k1); LOG("task2: RCV %d\n", e->x);
    }
    e->task.resume = 0; // HALT
}




/* Run until task3 halts and everything blocks on SND. */
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
    csp_cbuf_start(&s, &b, 1, 2, buf, sizeof(buf));
    csp_schedule(&s);
}
