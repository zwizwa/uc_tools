/* As a counterweight to synchronous csp.c, also implement the actor
   model of asynchronous concurrency.

   In the uc_tools library, there are two abstractions for buffers:

   - cbuf is a circular character token buffer, i.e. a byte stream

   - pbuf is a flat packet buffer

   Both are useful in practice, mostly bridging the token stream and
   packet levels of abstraction.  The main property of pbuf is the
   flat memory layout.  The main property of cbuf is that it can be
   used as a queue between interrupt and main loop on cortex M.


   To implement actors, the following abstractions are needed:

   - mailbox: a message queue

   - process: a state machine handling messages in a mailbox

   - scheduler: a program sequencing processes when they have messages
     in their mailboxes.


   The scheduler can be very simple: go over all processes in
   round-robin fashion and run them if they have data available.  My
   current (implicit) application just polls the queue explicitly,
   which is simple but wasteful.

   The main abstraction appears to be the message queue.  Starting
   from Erlang as my actor model example, the main question to ask is
   should out-of-order handling be supported?  In my current
   application I don't need it, so let's skip it for now.

   To make things a bit more concrete: a process is a struct that
   contains two members: a cbuf token stream and a resume function.
   And on top of that, tasks can be linked in a list to implement the
   scheduler's data structures (hot, cold and dead list).

*/

/* FIXME: Some issues with the current implementation:

   - How to allocate mailbox sizes?

   - Tasks should block when they try to write to a mailbox that is
     full, instead of dropping.

*/



#include "uc_tools_config.h" // for CBUF_DEBUG
#include "macros.h"
#include "dlist.h"
#include <stdint.h>

/* Parameterize everything over the message type, built from the
   ns_cbuf.h module. */
#include "ns_cbuf.h"

/* A task is a mailbox and a resume function, plus a doubly linked
   list used by the scheduler. */
struct NS(_task);
struct NS(_scheduler);

typedef void (*NS(_resume_f))(struct NS(_scheduler)*, struct NS(_task)*);
struct NS(_task) {
    struct dlist dlist;
    NS(_resume_f) resume;
    NS(_queue_t) *mbox;
    void *ctx;
};
typedef struct NS(_task) NS(_task_t);

/* Get a task at the top of one of the lists. */
static inline NS(_task_t) *NS(_dlist_top)(struct dlist *l) {
    /* Singleton means there is only the list sentinel. */
    if (dlist_singleton(l)) return NULL;
    /* Here we assume that dlist is the first member. */
    return (void*)(l->next);
}

/* A scheduler is a list of tasks.  Since we want to remove tasks, a
   head pointer in a separate struct is needed. */
struct NS(_scheduler) {
    struct dlist hot, cold, dead;
};
typedef struct NS(_scheduler) NS(_scheduler_t);

/* Move task to list. */
static inline void NS(_move)(NS(_task_t) *t, struct dlist *l) {
    dlist_move(&t->dlist, l);
}

/* Send message. We don't know what list the task is on, so remove
   from current list and insert into the hot list.  Most likely
   though, we are in the cold list so this is not too inefficient. */
static inline void NS(_send)(
    NS(_scheduler_t) *s, NS(_task_t) *t, NS(_element_t) msg) {
    NS(_move)(t, &s->hot);
    return NS(_put)(t->mbox, msg);
}

/* Halt is explicit. */
static inline void NS(_halt)(
    NS(_scheduler_t) *s, NS(_task_t) *t) {
    NS(_move)(t, &s->dead);
}

/* Perform a single tick on the first task in the hotlist. */
static inline NS(_task_t)* NS(_schedule_tick)(NS(_scheduler_t) *s) {
    /* The list head in the task struct is a sentinel. */
    NS(_task_t *t) = NS(_dlist_top)(&s->hot);
    if (!t) return NULL;
    LOG("schedule %p\n", t);
    /* Task is moved to cold list before resuming.  This means it will
       not be rescheduled until a new message arrives, so it better
       empty its mailbox on resume. */
    NS(_move)(t, &s->cold);
    t->resume(s, t);
    return t;
}

/* Run until no more hot tasks. */
static inline void NS(_schedule)(NS(_scheduler_t) *s) {
    int count = 10;
    while(NS(_schedule_tick(s))) {
        if (!(count--)) {
            ERROR("schedule loop\n");
        }
    }
}

/* Initialize scheduler dead list from task array. */
static inline void NS(_scheduler_init)(
    NS(_scheduler_t) *s, NS(_task_t) *t, uint32_t nb_tasks) {
    dlist_singleton_init(&s->hot);
    dlist_singleton_init(&s->cold);
    dlist_singleton_init(&s->dead);
    for (uint32_t i=0; i<nb_tasks; i++) {
        LOG("scheduler init %d %p\n", i, &t[i].dlist);
        dlist_singleton_init(&t[i].dlist);
        NS(_move)(&t[i], &s->dead);
    }
}

/* Spawn recycles a dead task. */
static inline NS(_task_t)* NS(_spawn)(
    NS(_scheduler_t) *s, NS(_queue_t) *mbox, NS(_resume_f) resume, void *ctx) {
    /* Move it to the hot list as it needs to resume at least once at startup. */
    NS(_task_t *t) = NS(_dlist_top)(&s->dead);
    ASSERT(t);
    NS(_move)(t, &s->hot);
    t->mbox = mbox;
    t->resume = resume;
    t->ctx = ctx;
    NS(_schedule)(s);
    return t;
}
