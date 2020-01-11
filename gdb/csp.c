/* Simple CSP scheduler

   The idea here is to strip down the idea to the bare minimum.  Some
   simplifications:

   - No priorities, no pre-emption, this is for main loop only.

   - Obviously no parallelism.  This is for small microcontrollers.

   - Interrupts can be supported through buffered channels.

   - Main loop can block on WFI and rerun the scheduler to empty
     buffers after interrupt.

   - Tasks are abstract.  This allows implementation of task to use
     the most convenient form, e.g. state machine with dispach, or
     CSP-like sequential programs.

   - Only a fixed number of static tasks are supported



   The basic structure of a CSP scheduler is quite straightforward.
   The central idea is rendez-vous: both SND and RCV need to block on
   a channel before they can both resume.

   This requres a round-robin scheduler with two queues, one for SND
   and one for RCV.  To schedule, execute the following loop:

     - For each RCV, find a corresponding SND.  If there are none,
       suspend the scheduler until a hardware event arrives.

     - If a SND/RCV pair is found, copy the channel value from the
       SND's context to the RCV's context and resume both tasks until
       they block again, re-queueing the continuations to the proper
       queues.  Then resume the scheduler loop.


   Some more notes:

     - SEL is like RCV, but will pick one of a number of channels



*/



#include <stdint.h>

struct csp_task;
typedef void (*csp_resume)(struct csp_task *k);

/* The scheduler sees tasks only as continuations, which are opaque C
   structures apart from a small header with bookkeping data.

   It is up to the user how to implement tasks.  E.g. manually coded
   state machines, or compiled into state machine form from sequential
   CSP-style code.

   Likewise, values communicated are also completely opaque.  Sender
   and receiver should implement type tags, or associate types to
   channels.
*/

struct csp_task {
    /* Multiple tasks (writers) can block on the same channel.  It's
       simplest to implement the pointers inside the task struct. */
    struct csp_task *next_task;

    void *buf;         // object being communicated in sender's memory
    uint16_t size;     // size of the object
    uint8_t chan;      // channel nb (or nb of channels for select)
    uint8_t op;        // channel op
    csp_resume resume; // resume task, updating the continuation in place
    uint8_t chans[0];  // channels to wait for in case of select
};

/* Semantics of send: by the time execution resumes, the object has
   been delivered to the receiver, and the storage for the data
   element can be reclaimed for private use.  The reverse for receive:
   on resume, object has been written. */
#define CSP_OP_SND 0
#define CSP_OP_RCV 1
#define CSP_OP_SEL 2
static inline void csp_op(uint8_t op, struct csp_task *k,
                          uint8_t chan, const void *buf, uint16_t size) {
    k->chan = chan;
    k->op   = op;
    k->size = size;
    k->buf  = (void*)buf;
}


/* E.g. a computed goto style state machine would have a pointer to
   store the resume point, and any other state data appended to the
   struct. */
struct csp_task_sm {
    struct csp_task k; // header for scheduler
    void *next;        // next pointer for CG machine
};


/* Optimize for fast switching.  One way to speed up is to implement
   channel->blocked_list mapping using an array.  It's not clear yet
   if this is a good or bad idea.  Depends on the application.  It
   might be good to make this a compile time configuration option.
   Memory-wise it seems fine, as all tasks will be blocking on some
   input channel when idle. */
#ifndef NB_CHANS
#define NB_CHANS 10
#endif

/* Queues.  Implement using cbuf? */
struct csp_scheduler {

    /* The "cold" list is a list of blocked tasks.  Currently
       implemented as a Channel -> task list mapping implemented as an
       array.  Each task in the domain of this mapping is waiting for
       a corresponding reader/writer to be rescheduled. */
    struct csp_task *channels[NB_CHANS];

    /* Current hot list (stack).  Each of these tasks need to be
       checked if they can rendez-vous with a blocked task, or if they
       can be moved to the cold list.  A hot list is necessary because
       unblocking a rendez-vous will generate two new continuations
       that will need to be checked. */
    struct csp_task *hot;
};

static inline struct csp_task *pop(struct csp_task **pstack) {
    struct csp_task *stack = *pstack;
    if (!stack) return 0;
    *pstack = stack->next_task;
    stack->next_task = 0;
    return stack;
}
static inline void push(struct csp_task **pstack, struct csp_task *task) {
    task->next_task = *pstack;
    *pstack = task;
}


/* Scheduler loop

Note that an external event buffer is not a real task, but we pretend
that a buffer for an input interrupt behaves as a task that has just
performed a write, and a buffer for an outgoing interrupt is a write
that does not cause any tasks to schedule and will only resume the
sender.

1. Precondition: everything is blocking.

2. Some external event happens (read or write interrupt).  The
   corresponding tasks are added to the hot list.

3. While there is a task on the hot list, find a corresponding blocked
   task on the cold list.

   3a.  There is no task.  Add the task to the cold list.

   3b.  There is a corresponding cold task.  Perform the data copy
        between writer and reader, resume both, and push their
        resulting suspension points to the hot list.
*/

static inline int is_rendezvous(struct csp_task *a,
                                struct csp_task *b) {
    /* FIXME: Extend for select. */
    return (!!a) && (!!b) && (a->op != b->op);
}
static inline struct csp_task **block_list(struct csp_scheduler *s, int c) {
    return &s->channels[c];
}

void csp_schedule(struct csp_scheduler *s) {
    struct csp_task *a, *b, **b_list;
    while((a = pop(&s->hot))) {
        b_list = block_list(s, a->chan);
        /* If there is more than one element in the block list, they
           must all be of the same type.  I.e. it's not possible that
           the first one hides a compatible one; it would have
           combined with it. */
        b = *b_list;
        if (is_rendezvous(a, b)) {
            pop(b_list);
            /* Resume both and push resulting ks to the hot list. */
            a->resume(a); push(&s->hot, a);
            b->resume(b); push(&s->hot, b);
        }
        else {
            /* Not a rendez-vous.  Store it in the cold list. */
            push(&s->channels[a->chan], a);
        }
    }
}
