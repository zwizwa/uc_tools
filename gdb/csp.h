#ifndef CSP_H
#define CSP_H

#include <stdint.h>

/* An event happens on a channel, and is associated with some data
   being transferred.  Direction is based on which array the event
   structure is stored in. */
struct csp_evt {
    uint16_t chan;
    uint16_t msg_len;
    void    *msg_buf;
};

/* Single-channel operation. */
static inline void csp_evt(struct csp_evt *o,
                          uint16_t chan,
                          void *msg_buf, uint32_t msg_len) {
    o->chan = chan;
    o->msg_buf = msg_buf;
    o->msg_len = msg_len;
}
#define CSP_EVT(task,n,ch,var) \
    csp_evt(&(task)->evt[n],ch,&(var),sizeof(var))


struct csp_task;
typedef void (*csp_resume_f)(struct csp_task *k);

struct csp_task {
    /* A task will be part of exactly one task list, so it's simplest
       to implement the list pointers inside the task struct. */
    struct csp_task *next_task;

    /* After a channel op completes, the code is resumed through this
     * callback.  The internals of a task are opaque. */
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

/* Single event send/receive */
static inline void csp_evt1(struct csp_task *t,
                            uint16_t chan,
                            void *msg_buf, uint32_t msg_len,
                            uint16_t nb_send,
                            uint16_t nb_recv) {
    csp_evt(&t->evt[0], chan, msg_buf, msg_len);
    t->nb_send = nb_send;
    t->nb_recv = nb_recv;
}


/* For use in computed goto machine.  See csv_test.c
   The task's struct will need to define the .next member.  See test code. */
#define CSP_EVT1_CG(e,ch,var,ns,nr,klabel)                           \
    e->next = &&klabel;                                                 \
    csp_evt1((struct csp_task *)(e),ch,&(var),sizeof(var),ns,nr);    \
    return;                                                             \
klabel:

#define CSP_SEND(e,ch,var,klabel) CSP_EVT1_CG(e,ch,var,1,0,klabel)
#define CSP_RECV(e,ch,var,klabel) CSP_EVT1_CG(e,ch,var,0,1,klabel)


struct csp_scheduler;

/* Run until all tasks are blocked. */
void csp_schedule(struct csp_scheduler *s);

/* Add a new task and schedule. */
void csp_start(struct csp_scheduler *s, struct csp_task *t);

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
                     struct csp_cbuf *b);
#endif

#endif
