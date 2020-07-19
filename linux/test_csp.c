#include "csp.h"
#include "csp_fd.h"
//#include "csp.c"
//#include "csp_fd.c"
#include "tcp_tools.h"

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

   The latter was my original intention, but has been postponed until
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
void test1(struct csp_scheduler *s) {
    struct state0 state0 = { .task = { .resume = (csp_resume_f)resume0 } };
    struct state1 state1 = { .task = { .resume = (csp_resume_f)resume1 } };
    struct state2 state2 = { .task = { .resume = (csp_resume_f)resume2 } };
    csp_start(s, &state0.task);
    csp_start(s, &state1.task);
    csp_start(s, &state2.task);
    csp_schedule(s);
}
/* Add something on a channel from the outside.  I.e. use the CSP
   network as a push reactive system.  Note that this requires a
   blocking receiving task to receive the value. */
void test2(struct csp_scheduler *s) {
    struct state1 state1 = { .task = { .resume = (csp_resume_f)resume1 } };
    struct state2 state2 = { .task = { .resume = (csp_resume_f)resume2 } };
    csp_start(s, &state1.task);
    csp_start(s, &state2.task);
    uint32_t inp = 0;
    for (int i=0; i<3; i++) {
        /* Sucessful send.  Channel 1 has reader. */
        ASSERT(1 == csp_send(s, 1, &inp, sizeof(inp)));
        /* Unsucessful send.  Channel 3 has no reader. */
        ASSERT(0 == csp_send(s, 3, &inp, sizeof(inp)));
        inp++;
    }
}
/* Test the buffered external input. */
void test3(struct csp_scheduler *s) {
    struct state2 state2 = { .task = { .resume = (csp_resume_f)resume2 } };
    csp_start(s, &state2.task);
    uint8_t buf[64];
    struct csp_async b;
    int ch_int = 1, ch_data = 2;
    csp_async_start(s, &b,
                    /* We're sending into the network*/
                    csp_async_send_task,
                    ch_int, ch_data, buf, sizeof(buf));
    uint8_t msg[] = {1,2,3};
    csp_async_write(s, &b, &msg, sizeof(msg));
    csp_async_notify(s, &b, 1);
    csp_async_write(s, &b, &msg, sizeof(msg));
    csp_async_notify(s, &b, 1);
}


/* Test csp_to_fd using socket output. */

// FIXME: implement
void test4_0(struct csp_scheduler *s) {
    int port = 1234;
    int fd = assert_tcp_connect("localhost", port);
    (void)fd;
    int chan = 0;
    int ext_chan = 123; // arbitrary

    /* This has a proper _init function. */
    struct csp_to_fd csp_to_fd;
    csp_to_fd_init(&csp_to_fd, fd, chan, ext_chan);
    csp_start(s, &csp_to_fd.task);

    uint32_t msg = 0x12345678;
    csp_send(s, chan, &msg, sizeof(msg));
}


void with_scheduler(int nb_c2e, int nb_c, void (*f)(struct csp_scheduler *)) {
    struct csp_scheduler s = {};
    struct csp_evt_list c2e[nb_c2e];
    struct csp_chan_to_evt c[nb_c];
    csp_scheduler_init(&s, c2e, nb_c2e, c, nb_c);
    f(&s);
}

int main(int argc, char **argv) {
    /* Sizes don't matter much for tests.  Just make sure they are
       large enough.  FIXME: create some functionality to compute
       storage parameters from application. */
    int nb_c2e = 20;
    int nb_c = 20;
    if (argc < 2) {
        LOG("- test1\n");
        with_scheduler(nb_c2e, nb_c, test1);
        LOG("- test2\n");
        with_scheduler(nb_c2e, nb_c, test2);
        LOG("- test3\n");
        with_scheduler(nb_c2e, nb_c, test3);
    }
    else {
        /* This test split over two linux processes. */
        LOG("- test4_0\n");
        if (atoi(argv[1]) == 0) {
            with_scheduler(nb_c2e, nb_c, test4_0);
        }
    }
}
