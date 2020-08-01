/* A minimal asynchronous actor implementation as an alternative to
   synchronous csp.c

   To implement actors, the following abstractions are needed:

   - mailbox: a message queue

   - task: a state machine handling messages in a mailbox when they
     come available + send new messages and spawn tasks.

   - scheduler: a program sequencing tasks whenever they have messages
     in their mailboxes.


   This makes some simplifications:

   - Mailboxes are ns_cbuf, parameterized by message type.

   - No out-of-order message handling (e.g. like Erlang's
     pattern-matching filtered receive).

   - Flat task struct with baked-in mailbox, memory externally managed

   - No handling of mailbox full condition at send: will drop
     messages instead.

*/

#include "macros.h"
#include "dlist.h"
#include <stdint.h>

/* Parameterize everything over the message type, built from the
   ns_cbuf.h module. */
#include "ns_cbuf.h"

/* A task is a mailbox and a resume function, plus a doubly linked
   list used by the scheduler to classify hot/cold/dead tasks. */
struct NS(_task);
struct NS(_scheduler);
typedef void (*NS(_resume_f))(struct NS(_scheduler)*, struct NS(_task)*);
struct NS(_task) {
    struct dlist dlist;
    NS(_resume_f) resume;
    NS(_queue_t) mbox;
};
struct NS(_scheduler) {
    struct dlist hot, cold, dead;
    int max_ticks;
};
typedef struct NS(_task) NS(_task_t);
typedef struct NS(_scheduler) NS(_scheduler_t);

/* Get a task at the top of one of the task lists. */
static inline NS(_task_t) *NS(_task_top)(struct dlist *l) {
    if (dlist_singleton(l)) return NULL;
    return (void*)(l->next);
}

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
    return NS(_put)(&t->mbox, msg);
}

/* Halt is explicit. */
static inline void NS(_halt)(
    NS(_scheduler_t) *s, NS(_task_t) *t) {
    NS(_move)(t, &s->dead);
}

/* Perform a single tick on the first task in the hotlist. */
static inline NS(_task_t)* NS(_schedule_tick)(NS(_scheduler_t) *s) {
    NS(_task_t *t) = NS(_task_top)(&s->hot);
    if (!t) return NULL;
    LOG("-- task %p begin\n", t);
    /* Task is moved to cold list before resuming.  This means it will
       not be rescheduled until a new message arrives, so it better
       empty its mailbox on resume. */
    NS(_move)(t, &s->cold);
    t->resume(s, t);
    LOG("-- task %p end\n", t);
    return t;
}

/* Run until no more hot tasks. */
static inline void NS(_schedule)(NS(_scheduler_t) *s) {
    LOG("- schedule begin\n");
    int count = s->max_ticks;
    while(NS(_schedule_tick(s))) {
        if (!(--count)) {
            ERROR("schedule loop\n");
        }
    }
    LOG("- schedule end\n");
}

/* Initialize scheduler dead list from task array. */
static inline void NS(_scheduler_init)(NS(_scheduler_t) *s) {
    dlist_init(&s->hot);
    dlist_init(&s->cold);
    dlist_init(&s->dead);
}

/* Spawn recycles a dead task.  This might not be the right
   abstraction.  In a scenario where static allocation is a must, it
   is probably best to let the caller provide the task struct. */
static inline NS(_task_t)* NS(_spawn)(NS(_scheduler_t) *s, NS(_task_t) *t) {
    ASSERT(t);
    NS(_move)(t, &s->hot);
    return t;
}

#ifndef NS_ACTOR_H
#define NS_ACTOR_H

/* This is untyped */
#define NS_ACTOR_TASK_INIT(field, resume_, mbuf) {            \
        .dlist = DLIST_INIT(field.dlist),                     \
        .resume = resume_,                                    \
        .mbox = { .buf = mbuf, .size = ARRAY_SIZE(mbuf) } }

#endif

