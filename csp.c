/* Simple CSP scheduler

   See csp.h for comments inline functions and data structures.
   See csp_test.c for examples

   General idea:

   - No priorities, no pre-emption.  This is for main loop only, and
     purely cooperative.  This is fine for a large class of
     non-CPU-bound "reactive" systems, but it is NOT an RTOS.

   - No multi-CPU support.  This is for small microcontrollers.

   - The scheduler essentially implements SELECT, waking up a task
     when exactly one of a collection of send/receive events has
     occured.  Single channel send and receive are a special case.

   - Tasks are abstract.  This allows implementation of task to use
     the most convenient form, e.g. state machine dispach function,
     computed goto, compiled from some other form (async/await style),
     or proper machine stack switching.

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

   TODO:

   - Replace the concrete copy operation by a transaction callback.
     During a rendez-vous, data can be made visible to two tasks.
     This would avoid message copy in case tasks run on the same
     machine.  Note that this might already be implemented correctly,
     by executing the receiver first.

   - Enforce directionality.  Together with a transaction mechanism,
     this allows packet transport of data payload over the network.

*/

#include "csp.h"

#ifndef ASSERT
#define ASSERT(x) if(!(x));
#endif

#ifndef LOG
#define LOG(...)
#endif

#if 1
#define LOG_DBG(...)
static inline void log_dbg_state(struct csp_scheduler *s) { }
#else
#define LOG_DBG LOG
/* Some static logging for debugging.  The datastructures are too low
   level to make sense of in the debugger. */
static inline void log_evt_list_tasks(struct csp_evt_list *l) {
    while(l) {
        /* Only print task for now. */
        LOG_DBG(" %p", l->key);
        l=l->next;
    }
    LOG_DBG("\n");
}
static void log_dbg_state(struct csp_scheduler *s) {
    /* Current test case has only one channel. */
    LOG_DBG("channel 0, cold tasks:\n");
    LOG_DBG(".send=0:"); log_evt_list_tasks(s->chan_to_evt[0].evts[0]);
    LOG_DBG(".recv=1:"); log_evt_list_tasks(s->chan_to_evt[0].evts[1]);
}
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

    /* Implement data transfer as copied or in-place shared access.
       Note that this needs to be conditional on presence of send
       buffer, as it is quite common to have empty events, e.g. just
       synchronization. */
    int shared_memory = 0;
    if (evt_send->msg_buf.v) {
        if (evt_recv->msg_buf.v) {
            uint32_t n = MIN(evt_send->msg_len, evt_recv->msg_len);
            memcpy(evt_recv->msg_buf.v, evt_send->msg_buf.v, n);
        }
        else {
            shared_memory = 1;
            evt_recv->msg_buf = evt_send->msg_buf;
            evt_recv->msg_len = evt_send->msg_len;
        }
    }

    /* In both tasks, mark which event has completed.  It is
       always exactly one event. */
    recv->selected = evt_recv - &recv->evt[0];
    send->selected = evt_send - &send->evt[0];

    /* Receiver is resumed first.

       1. It allows implementation of shared memory message passing,
          with the understanding that a receiver can only access the
          memory in the current time slot.

       2. It makes sends look more like function calls, which is
          useful for debugging as it makes traces more readable.  Note
          that the tasks themselves cannot rely on this knowledge.
    */
    if (CSP_WAITING != recv->resume(recv)) {
        recv->resume = 0;
    }
    if (shared_memory) {
        evt_recv->msg_buf.v = NULL;
    }
    if (CSP_WAITING != send->resume(send)) {
        send->resume = 0;
    }
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
    LOG_DBG("add_cold %p\n",cold);
    FOR_EVT_INDEX(e, cold) {
        int dir = task_evt_dir(cold, e);
        int ch = cold->evt[e].chan;
        ASSERT(ch < s->nb_chans);
        add_task(
            s, &(s->chan_to_evt[ch].evts[dir]),
            cold, &cold->evt[e]);
    }
}
static inline void add_hot(struct csp_scheduler *s,
                           struct csp_task *hot_task) {
    LOG_DBG("add_hot %p\n", hot_task);
    csp_task_push(&s->hot, hot_task);
}
static inline struct csp_task *pop_hot(struct csp_scheduler *s) {
    struct csp_task *task = csp_task_pop(&s->hot);
    LOG_DBG("pop_hot %p\n", task);
    return task;
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
    add_hot(s, t);
    csp_schedule(s);
}



/* Given a hot task, find a (any) cold task that can rendez-vous with
   one of the hot task's events and perform the rendez-vous.  If none,
   return NULL.

   This is a good place to reiterate the data structures.

   - Each task has an event list
   - Each event is associated to a channel
   - Each channel has a channel to events list for 2 directions (the cold list)

*/
static void schedule_task(
    struct csp_scheduler *s,
    struct csp_task *hot_task) {
    LOG_DBG("schedule_task %p\n", hot_task);
    log_dbg_state(s);
    FOR_EVT_INDEX(e, hot_task) {
        int cold_dir = !task_evt_dir(hot_task, e);
        int ch = hot_task->evt[e].chan;
        struct csp_evt_list *el = s->chan_to_evt[ch].evts[cold_dir];
        if (el) {
            struct csp_task *cold_task = el->key;
            struct csp_evt  *cold_evt  = el->evt;
            struct csp_evt  *hot_evt   = &hot_task->evt[e];

            ASSERT(cold_task != hot_task);

            /* Note that resume() will update the event structure to
               the next blocking point.  Anything that needs the
               current event structure will need to execute before
               that.  This includes remove_cold(), which needs the
               direction of the event to pick the correct cold list. */
            remove_cold(s, cold_task);

            if (cold_dir == 1) {
                // hot is send
                do_send(hot_task,  hot_evt,
                        cold_task, cold_evt);
            }
            else {
                // cold is send
                do_send(cold_task, cold_evt,
                        hot_task,  hot_evt);
            }

            /* Add both to hot list again if still active. */
            if(hot_task->resume)  add_hot(s, hot_task);
            if(cold_task->resume) add_hot(s, cold_task);
            return;
        }
    }
    /* None of the events have a corresponding cold task, so this
     * becomes a cold task. */
    add_cold(s, hot_task);
}

void csp_schedule(struct csp_scheduler *s) {
    struct csp_task *hot;
    while((hot = pop_hot(s))) {
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

/* Convenience routine to allocate scheduler on stack. */
void csp_with_scheduler(int nb_c2e, int nb_c, void (*f)(struct csp_scheduler *)) {
    struct csp_scheduler s = {};
    struct csp_evt_list c2e[nb_c2e];
    struct csp_chan_to_evt c[nb_c];
    csp_scheduler_init(&s, c2e, nb_c2e, c, nb_c);
    f(&s);
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

static csp_status_t resume_halt(struct csp_task *t) {
    /* FIXME: Currently halt is implemented as a side effect,
       Maybe just make it explicit? */
    t->resume = 0;
    return CSP_HALTED;
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
                  .msg_buf = { .v = msg_buf } }
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
                     csp_status_t (*task_resume)(struct csp_async *),
                     uint16_t c_int, uint16_t c_data,
                     void *buf, uint32_t size) {
    memset(b,0,sizeof(*b));
    cbuf_init(&b->cbuf, buf, size);
    b->c_int = c_int;
    b->c_data = c_data;
    b->task.resume = (csp_resume_f)task_resume;
    csp_start(s, &b->task);
    csp_schedule(s);
}

/* csp_async_start takes a task argument, which defines the flavour of
 * the object: send or receive.   These have directions switched. */
csp_status_t csp_async_send_task(struct csp_async *b) {
    if (b->next) goto *b->next;
    for(;;) {
        /* No data, only wait for interrupt to wake us up. */
        if (cbuf_empty(&b->cbuf)) {
            CSP_SYN(&(b->task), 0, b->c_int);                /* RCV */
            CSP_SEL(&(b->task), b, 0/*nb_send*/, 1/*nb_recv*/);
        }
        /* Data, wait for send to complete or interrupt to wake us up. */
        else {
            b->token = cbuf_peek(&b->cbuf, 0);
            CSP_EVT(&(b->task), 0, b->c_data, b->token);     /* SND */
            CSP_SYN(&(b->task), 1, b->c_int);                /* RCV */
            CSP_SEL(&(b->task), b, 1/*nb_send*/, 1/*nb_recv*/);
            if (0 == b->task.selected) {
                cbuf_drop(&b->cbuf, 1);
            }
            else {
                /* interrupt */
            }
        }
    }
}
/* Dual */
csp_status_t csp_async_recv_task(struct csp_async *b) {
    if (b->next) goto *b->next;
    for(;;) {
        /* Too much data, only wait for interrupt to wake us up so we can
           retry token receive. */
        if (cbuf_full(&b->cbuf)) {
            CSP_SYN(&(b->task), 0, b->c_int);                /* RCV */
            CSP_SEL(&(b->task), b, 0/*nb_send*/, 1/*nb_recv*/);
        }
        /* Data, wait for send to complete or interrupt to wake us up. */
        else {
            CSP_EVT(&(b->task), 0, b->c_data, b->token);     /* RCV */
            CSP_SYN(&(b->task), 1, b->c_int);                /* RCV */
            CSP_SEL(&(b->task), b, 0/*nb_send*/, 2/*nb_recv*/);
            if (0 == b->task.selected) {
                /* receive finished */
                ASSERT(b->token < 256);
                uint8_t byte = b->token;
                cbuf_write(&b->cbuf, &byte, 1);
            }
            else {
                /* interrupt */
            }
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


/* Bump allocator for scheduler data structures.

   It might be more convenient to not mess with pre-computing the
   memory usage and use a bump allocation where:

   - Memory that is no longer needed will be pushed into a free list
     for that particular type.

   - If a free list is empty, memory is taken from the bump allocator.

   It will still be possible to compute bounds, but in most cases the
   memory usage will stabilize over time.

*/


