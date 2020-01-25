/* Simple CSP scheduler

   See csp.h for comments inline functions and data structures.
   See csp_test.c for examples

   General idea:

   - No priorities, no pre-emption.  This is for main loop only, and
     purely cooperative.  This is fine for a large class of
     non-CPU-bound "reactive" systems.

   - No multi-CPU support.  This is for small microcontrollers.

   - The scheduler essentially implements SELECT, waking up a task
     when exactly one of a collection of send/receive events has
     occured.  Single channel send and receive are a special case.

   - Tasks are abstract.  This allows implementation of task to use
     the most convenient form, e.g. state machine dispach function,
     computed goto, or compiled from some other form (async/await
     style).

   - Interrupts can be supported through buffered channels, with WFI
     in the main loop to wake up the scheduler after interrupt.

   - Currently there is no limit on the amount of tasks.  Memory needs
     for the scheduler are bound by the number of channels, and the
     maximum number of simultaneous blocking events.

   - Values communicated over channels are opaque.  Sender and
     receiver should implement type tags, or associate types to
     channels.

   - Semantics of send/receive: by the time execution resumes, the
     object has been delivered to the receiver's buffer, and the
     storage for the data element can be reclaimed for private use at
     the sender's end.  No data is shared between tasks.

   A note on "real" tasks.  I see two options: either plug in a stack
   switcher, or go to Rust.  The intermediate sweet spot of using a
   "simple lisp" to compile to async tasks isn't easy to find, and
   might not really exist.  It would be nice to have something that
   compiles down to plain C.

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





/* Scheduler loop

Each csp_schedule() call runs until all tasks are blocked.  For this
to be useful in practice there has to be some external influence that
creates channel send or receive operations, opon which the scheduler
can be used to propagate through the network.

To simplify implementation, the schedule loop does not assume any
outside influence: it knows only about tasks, channels and select.
Any event coming from the outside is injected by emulating a task.
See e.g. csp_send() and csp_cbuf_send().

The general principle:

1. Precondition: everything is blocking.

2. Some external event happens (e.g. coming from a read or write
   interrupt on a microcontroller).  By some ad-hoc mechanism a task
   is added to the hot list.

3. csp_schedule() loop: while there is a task on the hot list, find a
   corresponding blocked task on the cold list

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



Implementation details

- In the current form there is no explicit "cold list".  It is
  implicit in the channel-indexed data structure.

- The index data structure can map a channel number to a task and
  corresponding event directly.

- The implementation uses plain pointers.  On a small RAM target,
  32bit pointers are wasteful.  This can probably be optimized.

- There is a memory pool for the linked lists.  An upper bound for its
  size is the sum of largest select statement for each task.
  Allocation is left to the library user.

- The hot/cold lists conceptually sets, i.e. there is is no inherent
  order.  However we do implement them using stacks which will result
  in some arbitrary priority.  Please do not rely on scheduling order.

*/











/* The csp_evt array has senders first, followed by receivers.  We use
 * 0 to indicate send, and 1 to indicate receive. */
static inline int task_evt_dir(struct csp_task *t, int e) {
    return e >= t->nb_send;
}
#define FOR_EVT_INDEX(e, t) \
    for(int e=0; e<t->nb_send+t->nb_recv; e++)

/* The hot list is implemented by chaining pointers in the task
   strcture.  We use ns_list.h "mixin" to implement csp_task_push()
   and csp_task_pop() */
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

    /* Receiver is resumed first.  This makes sends look more like
       function calls, which is useful for debugging as it makes
       traces more readable.  Note that the tasks cannot rely on this,
       and if they only use channel communication they don'tt even
       know about it. */
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

static void add_task(struct csp_scheduler *s,
                     struct csp_evt_list **l,
                     struct csp_task *t,
                     struct csp_evt *e) {
    ASSERT(t);
    struct csp_evt_list *tc = csp_task_op_pop(&s->memory_pool);
    tc->key = t;
    tc->evt = e;
    csp_task_op_push(l, tc);
}
static void remove_task(struct csp_scheduler *s,
                        struct csp_evt_list **l,
                        struct csp_task *t) {
    struct csp_evt_list *tc = csp_task_op_remove(l, t);
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
        remove_task(
            s, &(s->chan_to_evt[ch].evts[cold_dir]),
            cold);
    }
}
static inline void add_cold(
    struct csp_scheduler *s,
    struct csp_task *cold) {
    cold->selected = -1;
    FOR_EVT_INDEX(e, cold) {
        int dir = task_evt_dir(cold, e);
        int ch = cold->evt[e].chan;
        ASSERT(ch < s->nb_chans);
        add_task(
            s, &(s->chan_to_evt[ch].evts[dir]),
            cold, &cold->evt[e]);
    }
}
/* Given a hot task, find a (any) cold task that can rendez-vous with
   one of the hot task's events and perform the rendez-vous.  If none,
   return NULL. */
static void schedule_task(
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
        schedule_task(s, hot);
    }
}

/* Note on memory allocation: currently it is assumed that the
   scheduler is initialized with enough memory for chan->event map.
   Not clear how to make this more explicit. */
void csp_scheduler_init(
    struct csp_scheduler *s,
    struct csp_evt_list *e, int nb_e,
    struct csp_chan_to_evt *c, int nb_c) {
    memset(s,0,sizeof(*s));
    memset(e,0,sizeof(*e)*nb_e);
    memset(c,0,sizeof(*c)*nb_c);
    // Link the freelist
    csp_evt_list_init(e, nb_e);
    s->memory_pool = e;
    s->chan_to_evt = c;
    s->nb_chans = nb_c;
    s->hot = NULL;
}



/* External synchronous send & recv.

   This is a transaction from outside of the CSP network.  Note again
   that CSP is rendez-vous: when sending, there is no other place to
   put a value than inside a receiver's buffer, similar for receive:
   the other task needs to be sitting there with the message.

   If there is no peer currently blocked, there is no place to get or
   put the value and thus we let this function fail, as indicated by
   the return value.  See csp_cbuf_send() for a buffered send
   operation that does not have this drawback. */

static void resume_halt(struct csp_task *t) {
    t->resume = 0;
}
static int trans(struct csp_scheduler *s,
                 int chan,
                 int dir,
                 void *msg_buf,
                 uint32_t msg_len) {
    ASSERT(chan >= 0);
    ASSERT(chan < s->nb_chans);
    struct {
        struct csp_task task;
        struct csp_evt  evt;
    } t = {
        .task = { .resume  = resume_halt,
                  .nb_send = !dir,
                  .nb_recv = !!dir },
        .evt  = { .chan    = chan,
                  .msg_len = msg_len,
                  .msg_buf = msg_buf }
    };
    csp_task_push(&s->hot, &t.task);
    csp_schedule(s);

    /* If the transaction succeeded we're good. */
    if (t.task.selected == 0) return 1;

    /* If it didn't, we need to explicitly "kill" the task by removing
       it from the scheduler to avoid a dangling reference to the task
       object on this stack frame. */
    remove_cold(s, &t.task);
    return 0;
}
int csp_send(struct csp_scheduler *s,
             int chan,
             void *msg_buf,
             uint32_t msg_len) {
    return trans(s, chan, CSP_DIR_SEND, msg_buf, msg_len);
}
int csp_recv(struct csp_scheduler *s,
             int chan,
             void *msg_buf,
             uint32_t msg_len) {
    return trans(s, chan, CSP_DIR_RECV, msg_buf, msg_len);
}



/* External asynchronous send/receive.

   Compared to csp_send()/csp_recv(), this employs buffering to handle
   the case where the CSP network is not ready to send or receive.

   A persistent task monitors a notification channel and will perform
   a channel send or receive using the cbuf.

   Note that this is likely a little too simple and mostly serves as
   an example.  A straightforward extension is to transfer chunks or
   larger complete objects instead of bytes.  In practice i/o adapters
   at the edge of a CSP network will be likely ad-hoc.
*/

/* Start and object structure can be shared between send and receive
 * mode. */
void csp_async_start(struct csp_scheduler *s,
                    struct csp_async *b,
                    void (*task)(struct csp_async *),
                    uint16_t c_int, uint16_t c_data,
                    void *buf, uint32_t size) {
    memset(b,0,sizeof(*b));
    cbuf_init(&b->cbuf, buf, size);
    b->c_int = c_int;
    b->c_data = c_data;
    b->task.resume = (csp_resume_f)task;
    csp_start(s, &b->task);
    csp_schedule(s);
}

/* csp_async_start takes a task argument, which defines the flavour of
 * the object: send or receive.   These have directions switched. */
void csp_async_send_task(struct csp_async *b) {
    if (b->next) goto *b->next;
  again:
    /* No data, only wait for interrupt to wake us up. */
    if (cbuf_empty(&b->cbuf)) {
        CSP_SYN(b, 0, b->c_int);                /* RCV */
        CSP_SEL(b, 0/*nb_send*/, 1/*nb_recv*/);
        goto again;
    }
    /* Data, wait for send to complete or interrupt to wake us up. */
    else {
        b->token = cbuf_peek(&b->cbuf, 0);
        CSP_EVT(b, 0, b->c_data, b->token);     /* SND */
        CSP_SYN(b, 1, b->c_int);                /* RCV */
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
/* Dual */
void csp_async_recv_task(struct csp_async *b) {
    if (b->next) goto *b->next;
  again:
    /* Too much data, only wait for interrupt to wake us up so we can
       retry token receive. */
    if (cbuf_full(&b->cbuf)) {
        CSP_SYN(b, 0, b->c_int);                /* RCV */
        CSP_SEL(b, 0/*nb_send*/, 1/*nb_recv*/);
        goto again;
    }
    /* Data, wait for send to complete or interrupt to wake us up. */
    else {
        CSP_EVT(b, 0, b->c_data, b->token);     /* RCV */
        CSP_SYN(b, 1, b->c_int);                /* RCV */
        CSP_SEL(b, 0/*nb_send*/, 2/*nb_recv*/);
        switch(b->task.selected) {
        case 0: { /* receive finished */
            ASSERT(b->token < 256);
            uint8_t byte = b->token;
            cbuf_write(&b->cbuf, &byte, 1);
            goto again;
        }
        case 1: /* interrupt */
            goto again;
        }
    }
}


void csp_async_notify(struct csp_scheduler *s,
                      struct csp_async *b,
                      uint16_t nb) {
    /* Signal the reader/writer task, which is guaranteed to be
     * waiting for an interrupt. */
    ASSERT(1 == csp_send(s, b->c_int, NULL, 0));
}
int csp_async_write(struct csp_scheduler *s,
                    struct csp_async *b,
                    const void *data, uint32_t len) {
    if (cbuf_room(&b->cbuf) < len) return 0;
    cbuf_write(&b->cbuf, data, len);
    return 1;
}
int csp_async_read(struct csp_scheduler *s,
                   struct csp_async *b,
                   void *data, uint32_t len) {
    if (cbuf_bytes(&b->cbuf) < len) return 0;
    cbuf_read(&b->cbuf, data, len);
    return 1;
}

/* Interrupts.

   I can see two strategies to support interrupts.

   - If there is no danger for buffer overruns, run the CSP network to
     completion from the ISR through the external event mechanism.
     This requires all interrupts that participate to be part of the
     same priority level, so they will not pre-empt each other.  In
     this scenario, if an event cannot be handled, there is a natural
     place to drop it.  DMA can be used to decrease interrupt
     pressure.

   - If running the CSP network to completion is not possible before a
     new interrupt needs to be handled, and DMA cannot reduce
     pressure, it is possible to use two effective priority levels:

     - Read/write buffer from ISR (not a CSP network op)

     - Poll buffers from main loop after WFI (CSP sync message +
       schedule until completion).

       This can even use a single event buffer which makes polling
       much simpler.

   See os.c for an example.

*/


