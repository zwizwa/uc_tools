/* Simple CSP scheduler
   See csp.c for more comments. */

#ifndef CSP_H
#define CSP_H

#include "cbuf.h"
#include "gensym.h"

#include <stdint.h>
#include <string.h>

/* An event happens on a channel, and is associated with some data
   being transferred.  Direction is based on which array the event
   structure is stored in. */
struct csp_evt {
    uint16_t chan;
    uint16_t msg_len;
    void    *msg_buf;
};

/* Initialize a single-channel operation. */
static inline void csp_evt(struct csp_evt *o,
                           uint16_t chan,
                           void *msg_buf, uint32_t msg_len) {
    o->chan = chan;
    o->msg_buf = msg_buf;
    o->msg_len = msg_len;
}


struct csp_task;
typedef void (*csp_resume_f)(struct csp_task *k);

struct csp_task {
    /* A task will be part of exactly one task list, so it's simplest
       to implement the list pointers inside the task struct. */
    struct csp_task *next;

    /* After a channel op completes, the code is resumed through this
     * callback.  The internals of a task are opaque.  Set this to
     * NULL to indicate a halted task. */
    csp_resume_f resume;

    /* Each task blocks on a select operation, which can contain a
       mixture of sends and receives.  A select cotnains nb_send send
       ops and nb_receive receive ops.  There is always at least one
       op.  User needs allocate as many slots as are necessary
       throughout the program. */
    uint8_t nb_send;
    uint8_t nb_recv;
    uint8_t selected;
    uint8_t _res;
    struct csp_evt evt[];
};

/* For use in computed goto machines. See example below.  Note that
   the scheduler doesn't care how a task is implemented.  It only
   knows about 'resume'.  However, we do need to provide a couple of
   tasks in the library, so the Computed Goto (CG) approach is used.

   Some assumptions to keep the macros simple, state struct has:
   - csp_task member named 'task'
   - next code pointer 'next' */

/* Select is the main blocking operation.  It waits for any of a set
   of channel operations (events) to complete.  Before calling
   CSP_SELECT, the corresponding csp_evt structures will need to be
   initialized. */

#define CSP_SEL_K(state,ns,nr,klabel) \
    (state)->task.nb_send = ns;       \
    (state)->task.nb_recv = nr;       \
    (state)->next = &&klabel;         \
    return;                           \
klabel:
#define CSP_SEL(state,ns,nr) \
    CSP_SEL_K(state,ns,nr,GENSYM(resume_))

/* Data transfer event. */
#define CSP_EVT(state,n,ch,var) \
    csp_evt(&((state)->task.evt[n]),ch,&(var),sizeof(var))

/* Single op send and receive are special cases of select */
#define CSP_SND(state,ch,var)                   \
    do { CSP_EVT(state,0,ch,var);               \
         CSP_SEL(state,1,0); } while(0)
#define CSP_RCV(state,ch,var)                   \
    do { CSP_EVT(state,0,ch,var);               \
         CSP_SEL(state,0,1); } while(0)


struct csp_scheduler;

/* Run until all tasks are blocked. */
void csp_schedule(struct csp_scheduler *s);

/* Add a new task and schedule. */
void csp_start(struct csp_scheduler *s, struct csp_task *t);


#if 1
/* Example of a Computed Goto (CG) machine with one event buffer,
 * going through variable initialization and then sending out the
 * variable's value, alternating channel 1 and 2 */
struct csp_cg_example {
    struct csp_task task;
    struct csp_evt evt[1];
    void *next;
    uint32_t a;
};
void csp_cg_example_resume(struct csp_cg_example *t /* task state */) {
    if (t->next) goto *t->next;
    t->a = 123;
    for(;;) {
        CSP_SND(t, 1, t->a);
        CSP_SND(t, 2, t->a);
    }
}
void csp_cg_example_start(struct csp_scheduler *s, struct csp_cg_example *t) {
    memset(t,0,sizeof(*t));
    t->task.resume = (csp_resume_f)csp_cg_example_resume;
    csp_start(s, &t->task);
}
#endif





/* Send something to a channel from the outside of the CSP network.
   This can fail if there is no task waiting on the channel. */
int csp_send(struct csp_scheduler *s,
             int chan,
             void *msg_buf,
             uint32_t msg_len);




#ifdef CBUF_H
/* Contrasted with csp_send(), which can fail if there is no task
   waiting.  csp_cbuf_send() will place data in a buffer as long as
   there is room.  A CSP task then handles transfer from the cbuf into
   the CSP network. */
struct csp_cbuf {
    struct csp_task task;
    struct csp_evt evt[2]; // need to watch send and receive
    struct cbuf cbuf;
    void *next;
    uint16_t token;
    uint16_t nb_bytes;
    uint16_t c_int;
    uint16_t c_data;
};
void csp_cbuf_start(struct csp_scheduler *s,
                    struct csp_cbuf *b,
                    uint16_t c_int, uint16_t c_data,
                    void *buf, uint32_t size);
/* Write (async) and notify (sync) */
int csp_cbuf_write(struct csp_scheduler *s,
                   struct csp_cbuf *b,
                   void *data, uint32_t len);
void csp_cbuf_notify(struct csp_scheduler *s,
                     struct csp_cbuf *b,
                     uint16_t nb);
#endif

#endif
