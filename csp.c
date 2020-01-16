/* Simple CSP scheduler

   See csp.h for comments inline functions and data structures.
   See csp_test.c for examples

   This structs down the idea.  Some simplifications:

   - No priorities, no pre-emption.  This is for main loop only, and
     purely cooperative.  This is fine for a large class of
     non-CPU-bound "reactive" systems.

   - No multi-CPU support.  This is for small microcontrollers.

   - Tasks are abstract.  This allows implementation of task to use
     the most convenient form, e.g. state machine dispach function,
     computed goto, or some form compiled down from a more traditional
     CSP-like sequential language.

   - Interrupts can be supported through buffered channels and WFI in
     the main loop to wake up the scheduler.

   - Only a fixed number of static tasks are supported

   - Channels are just identifiers.  Currently there is 16 bit of
     channel space.

   The scheduler essentially implements SELECT, waking up a task when
   one of a collection of send/receive events has occured.

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

#define FOR_EVT_INDEX(e, t) \
    for(int e=0; e<t->nb_send+t->nb_recv; e++)


static inline int task_evt_dir(struct csp_task *t, int e) {
    return e >= t->nb_send;
}

/* Tasks are only in one list at a time (hot or cold).  This allows
   the list chaining pointer to be included in the task strcture.
   This uses ns_list.h "mixin" to implement
   csp_task_push() and csp_task_pop() */
typedef struct csp_task csp_task_list_t;
#define NS(name) CONCAT(csp_task,name)
#include "ns_list.h"
#undef NS

/* Iterate over a task list.  Uses ** to allow for pop/push. */
#define FOR_TASKS(pp,list) \
    for(struct csp_task **pp = &(list); *pp; pp = &((*pp)->next))


static inline void do_send(
    struct csp_task *send, struct csp_evt *evt_send,
    struct csp_task *recv, struct csp_evt *evt_recv) {

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

    recv->resume(recv);
    send->resume(send);
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
    csp_task_push(&s->hot, t);
    csp_schedule(s);
}





/* Indexed lookup  (TODO)

   It's possible to create a faster scheduler by using an index
   structure that can map a channel number to a task directly,
   avoiding the search in the simpler implementation.

   First attempt to implement this uses plain pointers.  ( It is
   probably possible to optimize memory use by using object indices
   instead of pointers. )

   The size of the index is bounded by:

   - One channel_to_task record per channel (12 bytes)

   - One csp_task_list cell for each blocked event (8 bytes).  The
     upper bound is the max blocked event count per task.

   Basic operation:

   - On suspend, add the task to the corresponding channel_to_task
     entry using

   - On resume, remove ...

   - To implement the lists linearly, a free list needs to be stored
     somewhere to implement add/remove as a push+pop combo to the free
     list.


*/


/* csp_task_op_push, _pop, _remove come from ns_key_list.h */
typedef struct csp_evt_list  csp_task_op_list_t;
typedef struct csp_task*     csp_task_op_key_t;
#define NS(name) CONCAT(csp_task_op,name)
#include "ns_key_list.h"
#undef NS



/* Initialize the free list from an array. */
void csp_evt_list_init(struct csp_evt_list *l, int n) {
    for (int i=0; i<n; i++) {
        l[i].key = NULL;
        l[i].next = &l[i+1];
    }
    l[n-1].next = NULL;
}

void csp_schedule_add_task(struct csp_scheduler *s,
                            struct csp_evt_list **l,
                            struct csp_task *t,
                            struct csp_evt *e) {
    ASSERT(t);
    struct csp_evt_list *tc = csp_task_op_pop(&s->memory_pool);
    tc->key = t;
    tc->evt = e;
    csp_task_op_push(l, tc);
}
void csp_schedule_remove_task(struct csp_scheduler *s,
                               struct csp_evt_list **l,
                               struct csp_task *t) {
    struct csp_evt_list *tc = csp_task_op_remove(l, t);
    if (tc) csp_task_op_push(&s->memory_pool, tc);
}
void csp_schedule_drop_task(
    struct csp_scheduler *s,
    struct csp_evt_list **l) {

    struct csp_evt_list *tc = csp_task_op_pop(l);
    if (tc) csp_task_op_push(&s->memory_pool, tc);
}

/* A cold task is blocked in a select, waiting for a number of
   channels.  Each channel has 2 wait lists, one for send and one for
   receive.  Entries in that list point to task and event, so they can
   be woken up immediately if a rendez-vous is found. */
static inline void remove_cold(
    struct csp_scheduler *s,
    struct csp_task *cold) {
    FOR_EVT_INDEX(e, cold) {
        int cold_dir = task_evt_dir(cold, e);
        int ch = cold->evt[e].chan;
        csp_schedule_remove_task(
            s, &(s->chan_to_evt[ch].evts[cold_dir]),
            cold);
    }
}
static inline void add_cold(
    struct csp_scheduler *s,
    struct csp_task *cold) {
    FOR_EVT_INDEX(e, cold) {
        int dir = task_evt_dir(cold, e);
        int ch = cold->evt[e].chan;
        ASSERT(ch < s->nb_chans);
        csp_schedule_add_task(
            s, &(s->chan_to_evt[ch].evts[dir]),
            cold, &cold->evt[e]);
    }
}
/* Given a hot task, find a (any) cold task that can rendez-vous with
   one of the hot task's events and perform the rendez-vous.  If none,
   return NULL. */
static void schedule(
    struct csp_scheduler *s,
    struct csp_task *hot) {
    FOR_EVT_INDEX(e, hot) {
        int cold_dir = !task_evt_dir(hot, e);
        int ch = hot->evt[e].chan;
        struct csp_evt_list *el = s->chan_to_evt[ch].evts[cold_dir];
        if (el) {
            struct csp_task *cold     = el->key;
            struct csp_evt  *cold_evt = el->evt;
            struct csp_evt  *hot_evt  = &hot->evt[e];
            if (cold_dir == 1) {
                // hot is send
                do_send(hot,  hot_evt,
                        cold, cold_evt);
            }
            else {
                // cold is send
                do_send(cold, cold_evt,
                        hot,  hot_evt);
            }
            remove_cold(s, cold);

            /* Add both to hot list again. */
            if(hot->resume)  csp_task_push(&s->hot, hot);
            if(cold->resume) csp_task_push(&s->hot, cold);
            return;
        }
    }
    /* None of the events have a corresponding cold task, so this
     * becomes a cold task. */
    add_cold(s, hot);
}



void csp_schedule(struct csp_scheduler *s) {
    struct csp_task *hot;
    while((hot = csp_task_pop(&s->hot))) {
        schedule(s, hot);
    }
}

/* Note on memory allocation: currently it is assumed that the
   scheduler is initialized with enough memory for chan->event map.
   Not clear how to make this more explicit. */
void csp_scheduler_init(
    struct csp_scheduler *s,
    struct csp_evt_list *c2e, int nb_c2e,
    struct csp_chan_to_evt *c, int nb_c) {
    memset(s,0,sizeof(*s));
    memset(c2e,0,sizeof(*c2e)*nb_c2e);
    memset(c,0,sizeof(*c)*nb_c);
    // Link the freelist
    csp_evt_list_init(c2e, nb_c2e);
    s->memory_pool = c2e;
    s->chan_to_evt = c;
    s->nb_chans = nb_c;
    s->hot = NULL;
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
    ASSERT(chan >= 0);
    ASSERT(chan < s->nb_chans);
    struct {
        struct csp_task task;
        struct csp_evt  evt;
    } t = {
        .task = { .resume  = csp_send_resume,
                  .nb_send = 1,
                  .nb_recv = 0 },
        .evt  = { .chan    = chan,
                  .msg_len = msg_len,
                  .msg_buf = msg_buf }
    };
    t.task.selected = -1;
    csp_task_push(&s->hot, &t.task);
    csp_schedule(s);
    if (t.task.selected == 0) return 1; // FIXME

    /* Hot list is now empty.

       Next we need to guarantee that the cold list does not contain
       any reference to to t.  This is possible if the send event was
       not matched with a receive event. */
    remove_cold(s, &t.task);
    return 0;
}


/* External asynchronous send.

   Compared to csp_send(), a external buffered send will succeed as
   long as the buffer doesn't overflow.  It requires a persistent task
   to monitor the buffer, and a transient task to signal the reader
   that new data has arrived. */

void csp_cbuf_task(struct csp_cbuf *b) {
    if (b->next) goto *b->next;
  again:
    /* No data, only wait for interrupt to wake us up. */
    if (cbuf_empty(&b->cbuf)) {
        CSP_RCV(b, b->c_int, b->nb_bytes);
        goto again;
    }
    /* Data, wait for send to complete or interrupt to wake us up. */
    else {
        b->token = cbuf_peek(&b->cbuf, 0);
        CSP_EVT(b, 0, b->c_data, b->token);     /* SND */
        CSP_EVT(b, 1, b->c_int,  b->nb_bytes);  /* RCV */
        CSP_SEL(b, 1/*nb_send*/, 1/*nb_recv*/);
        switch(b->task.selected) {
        case 0: /* send finished */
            cbuf_drop(&b->cbuf, 1);
            goto again;
        case 1: /* interrupt */
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


