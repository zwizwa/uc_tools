/* Simple CSP scheduler

   See csp.h for comments inline functions and data structures.
   See csp_test.c for examples

   This structs down the idea.  Some simplifications:

   - No priorities, no pre-emption.  This is for main loop only, and
     purely cooperative.  This is fine for a large class of
     non-CPU-bound "reactive" systems.

   - No multi-CPU support.  This is for small microcontrollers.

   - Tasks are abstract.  This allows implementation of task to use
     the most convenient form, e.g. state machine with dispach,
     computed goto, or some form compiled down from a more traditional
     CSP-like sequential language.

   - Interrupts can be supported through buffered channels and WFI in
     the main loop to wake up the scheduler.

   - Only a fixed number of static tasks are supported

   - Channels are just identifiers.  Currently there is 16 bit of
     channel space.

   - The scheduler essentially implements SELECT, waking up a task
     when one of a collection of send/receive events has occured.

*/

#include "csp.h"

#ifndef ASSERT
#define ASSERT(x) if(!(x));
#endif

#ifndef LOG
#define LOG(...)
#endif

#include <stdint.h>
#include <string.h>


/* The scheduler sees tasks only as continuations, which are opaque C
   structures apart from a small header with bookkeping data.

   It is up to the user how to implement tasks.  E.g. manually coded
   state machines, or compiled into state machine form from sequential
   CSP-style code.  We use computed goto style tasks, and provide some
   macros for this style.  See csp.h

   Values communicated are also completely opaque.  Sender and
   receiver should implement type tags, or associate types to
   channels.
*/


/* Semantics of send/receive: by the time execution resumes, the
   object has been delivered to the receiver's buffer, and the storage
   for the data element can be reclaimed for private use at the
   sender's end.

   Single channel send and receive are implemented in terms of the
   more general select.

   Select returns when one and only one of its events has occured, and
   'selected' points to the event index.
*/










/* Scheduler loop

Each csp_schedule() call runs until all tasks are blocked.  For this
to be useful in practice there has to be some external influence that
creates new channel operations, opon which the scheduler can be used
to propagate through the network.

To simplify impelementation, the schedule loop does not assume any
outside influence: it knows only about tasks, channels and select.
Any event coming from the outside is injected by emulating a task.
See e.g. csp_send() and csp_cbuf_send().

The general principle:

1. Precondition: everything is blocking.

2. Some external event happens (e.g. coming from a read or write
   interrupt on a microcontroller).  By some ad-hoc mechanism a task
   is added to the hot list.

3. csp_schedule() loop: while there is a task on the hot list, find a
   corresponding blocked task on the cold list.

   3a.  There is no corresping task. Add the hot task to the cold
        list.

   3b.  There is a corresponding cold task.  Remove it from the cold
        list. Perform the data copy between writer and reader, resume
        both, and push their resulting suspension points to the hot
        list.

4. In case a temporary task was created to perform an interaction with
   the CSP network, it can be removed from the cold list at this time.
   This is necessary in case the temporary task was allocated on the C
   stack.

*/


/* Some implementation details:

   - A task is either hot or cold.  Cold tasks are tasks that were
     blocked in a previous scheduler run.  Hot tasks can potentially
     rendez-vous with cold tasks.

   - Every blocking action is a select.  Single channel send and
     receive are special cases of only one blocking action.  Upon
     resume, exactly one event has completed.

   - Each task points to a select, which is a set of send and a set of
     receive operations, each with its corresponding channel reference
     and message data slot.

   - The data structures are conceptually sets, i.e. there is is no
     inherent order.  However we do implement them using stacks.

*/



/* The csp_evt array has senders first, followed by receivers. */
static inline struct csp_evt *task_evt_send(struct csp_task *s) {
    return &s->evt[0];
}
static inline struct csp_evt *task_evt_recv(struct csp_task *s) {
    return &s->evt[s->nb_send];
}
/* Corresponding iterations */
#define FOR_SEND_EVT(o, t) \
    for (struct csp_evt *o = task_evt_send(t); \
         o < (task_evt_send(t) + t->nb_send); \
         o++)
#define FOR_RECV_EVT(o, t) \
    for (struct csp_evt *o = task_evt_recv(t); \
         o < (task_evt_recv(t) + t->nb_recv); \
         o++)


/* Tasks are only in one list at a time (hot or cold).  This allows
 * the list chaining pointer to be included in the task strcture. */
static inline struct csp_task *task_pop(struct csp_task **pstack) {
    struct csp_task *stack = *pstack;
    if (!stack) return 0;
    *pstack = stack->next_task;
    stack->next_task = 0; // POST: task is not in any list
    return stack;
}
static inline void task_push(struct csp_task **pstack, struct csp_task *task) {
    ASSERT(!task->next_task); // PRE: task is not in any list
    task->next_task = *pstack;
    *pstack = task;
}
/* Iterate over a task list.  Uses ** to allow for pop/push. */
#define FOR_TASKS(pp,list) \
    for(struct csp_task **pp = &(list); *pp; pp = &((*pp)->next_task))


/* Task sets, implemented as stacks. */
struct csp_scheduler {
    struct csp_task *cold; // blocked, when not considering hot tasks
    struct csp_task *hot;  // can unblock a cold task, or become a cold task
};



/* Check if two tasks can rendez-vous as sender and receiver.
   Update scheduler structure accordingly.
   See csp_schedule() first. */
static inline int maybe_resume(
    struct csp_scheduler *sched,
    struct csp_task **cold_list_entry,
    struct csp_task *send,
    struct csp_task *recv) {

    /* Scan all combinations of send and receive events until there is
     * a match. */
    FOR_SEND_EVT(evt_send, send) {
    FOR_RECV_EVT(evt_recv, recv) {
    if (evt_send->chan == evt_recv->chan) {

        /* Rendez-vous found */

        /* Tasks can be removed from lists.  Hot list entry has
           already been removed by caller.  Remove cold list entry for
           the other task here.  This has to be done before adding the
           task to another list.  We don't know if this is the select
           or the send, so let caller pass in this extra reference. */
        task_pop(cold_list_entry);

        /* If there is data, copy it over the channel, truncating if
           necessary. */
        if (evt_recv->msg_len) {
            uint32_t n =
                (evt_recv->msg_len >= evt_send->msg_len) ?
                evt_send->msg_len : evt_recv->msg_len;
            memcpy(evt_recv->msg_buf, evt_send->msg_buf, n);
            evt_recv->msg_len = n;
        }

        /* In both tasks, mark which event has completed.  It is
           always exactly one event. */
        recv->selected = evt_recv - &recv->evt[0];
        send->selected = evt_send - &send->evt[0];

        /* Resume both tasks, and push the resulting continuations to
           the hot list for further evaluation.  NULL resume means
           halt after this op, which is implemented by not
           rescheduling.  Resume receiver first.  Conceptually there
           is no order, but doing it that way makes debug traces look
           more like function call chains for SND->RCV. */
        recv->resume(recv);
        if (recv->resume) {
            task_push(&sched->hot, recv);
        }
        send->resume(send);
        if (send->resume) {
            task_push(&sched->hot, send);
        }

        //LOG("!");
        return 1;
    }}}
    //LOG(".");
    /* No Rendez-vous found. */
    return 0;
}

void csp_schedule(struct csp_scheduler *s) {
    struct csp_task *hot;

    /* Repeat until hot list is empty. */
  next_hot:
    if (!(hot = task_pop(&s->hot))) return;

    //LOG("s %p\n", s);
    FOR_TASKS(pcold, s->cold) {
        //LOG("pcold %p\n", pcold);
        struct csp_task *cold = *pcold;
        /* Check if these two tasks can rendez-vous.  If so, task
         * lists are updated by maybe_resume() so we can continue. */
        if (maybe_resume(s, pcold, hot, cold)) goto next_hot;
        if (maybe_resume(s, pcold, cold, hot)) goto next_hot;
    }
    /* If there is no match, no structures were updated and we can
     * consider the task blocked. */
    task_push(&s->cold, hot);
    goto next_hot;
}




void csp_start(struct csp_scheduler *s, struct csp_task *t) {
    t->resume(t);
    /* Task went through its initialization code and is now halted, or
       blocked on select.  If it halts immediately no communication
       has happened, which means it cannot affect the CSP network, so
       we're done. */
    if (!t->resume) return;

    /* Otherwise one of the select events can have an effect on the
       network.  Propagate until everything is blocked again. */
    task_push(&s->hot, t);
    csp_schedule(s);
}



/* External synchronous send.

   This is a send from outside of the CSP network.  Note again that
   CSP is rendez-vous: when sending, there is no other place to put a
   value than inside a receiver's buffer.  If there is no receiver
   currently blocked, there is no place to put the value and thus we
   let this function fail, as indicated by the return value.  See
   csp_cbuf_send() for a buffered send operation that does not have
   this drawback.

   External receive would be dual to this, but in practice most
   systems are push systems, and CSP network output can often be
   direct procedure calls or buffer writes. */
static void csp_send_resume(struct csp_task *t) {
    t->resume = 0;
}
int csp_send(struct csp_scheduler *s,
             int chan,
             void *msg_buf,
             uint32_t msg_len) {
    struct {
        struct csp_task task;
        struct csp_evt op;
    } t = {
        .task = {
            .resume = csp_send_resume,
            .nb_send = 1,
            .nb_recv = 0,
        },
        .op = {
            .chan = chan,
            .msg_len = msg_len,
            .msg_buf = msg_buf
        }
    };
    task_push(&s->hot, &t.task);
    csp_schedule(s);

    /* Hot list is now empty.

       Next we need to guarantee that the cold list does not contain
       any reference to to t.  This is possible if the send event was
       not matched with a receive event. */
    FOR_TASKS(pt, s->cold) {
        if (*pt == &t.task) {
            task_pop(pt);
            return 0;
        }
    }
    return 1;
}


/* External asynchronous send.

   Compared to csp_send(), a external buffered send will succeed as
   long as the buffer doesn't overflow.  It requires a persistent task
   to monitor the buffer, and a transient task to signal the reader
   that new data has arrived. */
#ifdef CBUF_H
void csp_cbuf_task(struct csp_cbuf *b) {
    if (b->next) goto *b->next;
  again:
    if (cbuf_empty(&b->cbuf)) {
        /* Wait for send and interrupt. */
        //LOG("cbuf: sel: int wait\n");
        CSP_RCV(b, b->c_int, b->nb_bytes);
        //LOG("cbuf: sel: int cont\n");
        goto again;
    }
    else {
        b->token = cbuf_peek(&b->cbuf, 0);
        /* Wait for send and interrupt. */
        //LOG("cbuf: sel: int,data wait\n");
        CSP_EVT(b, 0, b->c_data, b->token);     /* SND */
        CSP_EVT(b, 1, b->c_int,  b->nb_bytes);  /* RCV */
        CSP_SEL(b, 1/*nb_send*/, 1/*nb_recv*/);
        switch(b->task.selected) {
        case 0: /* send finished */
            //LOG("cbuf: sel: int,data data cont\n");
            cbuf_drop(&b->cbuf, 1);
            goto again;
        case 1: /* interrupt */
            //LOG("cbuf: sel: int,data int cont\n");
            goto again;
        }
    }
}
void csp_cbuf_start(struct csp_scheduler *s,
                    struct csp_cbuf *b,
                    uint16_t c_int, uint16_t c_data,
                    void *buf, uint32_t size) {
    memset(b,0,sizeof(*b));
    cbuf_init(&b->cbuf, buf, size);
    b->c_int = c_int;
    b->c_data = c_data;
    b->task.resume = (csp_resume_f)csp_cbuf_task;
    csp_start(s, &b->task);
    csp_schedule(s);
}
void csp_cbuf_notify(struct csp_scheduler *s,
                     struct csp_cbuf *b,
                     uint16_t nb) {
    /* Signal the reader task, which is guaranteed to be waiting for
     * an interrupt. */
    ASSERT(1 == csp_send(s, b->c_int, &nb, sizeof(nb)));
}
int csp_cbuf_write(struct csp_scheduler *s,
                   struct csp_cbuf *b,
                   void *data, uint32_t len) {
    if (cbuf_room(&b->cbuf) < len) return 0;
    cbuf_write(&b->cbuf, data, len);
    return 1;
}
#endif
