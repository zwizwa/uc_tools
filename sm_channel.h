#ifndef SM_EVENT_H
#define SM_EVENT_H

/* An implementation of transactional channels for "poll-style" state
   machines.

   This abstraction resembles CSP-style synchronous non-buffered
   channels (as opposed to say Actor-style mailboxes).  Avoiding
   buffering is an essential requirement in the use case this was
   constructed for.


   But first: why use poll-style machines in the first place?  Why not
   build an entire application on top of a CSP style scheduler, and
   have the correct semantics by design?

   The reason is implementation simplicity.  If power consumption is
   not an issue, a busy poll loop avoids the need of a scheduler and
   the associated explicit representation of events.

   The simplicity comes from the property that synchronization can
   happen based on conditions evaluated locally by the tasks.  E.g. a
   task blocks on:

       while (!condition()) { suspend(); }

   Inversion of control is not necssary.  I.e. events do not need to
   be turned into callbacks or ready queues.  This can save a
   substantial amount of code and bookkeeping data.

   Note that semantically, there is no essential difference: events
   also represent conditions.  It is just that representation in C
   becomes very straightforward.  I.e. the line above would be
   equivalent to a blocking call:

       receive_event();

   Assuming that this establishes why 1. we would want to use
   poll-style machines, and 2. that we are still dealing with events
   abstractly, let's move on to the actual problem:

       How to interface such a system to a push-style system?

   With push-style we mean callbacks that present data that is live
   during the extent of a C function call, allowing the associated
   resources to be allocated on the C stack.

   The interaction between a push call and a task is as follows:

   - Data is ready, and needs to be processed immediately

   - It is passed to a task (something with execution context, e.g. a
     state machine or a process with a stack).

   - Passing the data causes execution to continue in the task, and
     the data will either be consumed or not before the task blocks
     again waiting on some condition.

   At the push end we want to know if task consumed the data or not,
   so we can make a decision based on that.  The primitive operation
   to do this is the non-blocking write that either succeeds or
   doesn't.

   That is the purpose of this file: to establish an interface to
   perform these kinds of conditional transactions.

   The added benefit is that the conditional transaction can then also
   be used as a communication primitive between poll-style machines,
   where the writer can wait for the condition that a transaction is
   completed, essentially implementing CSP.

   Note however that this abstraction is not entirely the same as a
   CSP channel.  Here is a counterexample: task A writes to task B,
   task B reads and re-uses the same channel to write to task A, and
   task A reads again.  This is valid in CSP, but cannot be
   represented in the model in this file without extra annotation,
   e.g. a transaction counter.  We stick to the simple implementation
   and add the additional constraint that a channel is unidirectional,
   which allows changes betwen NULL and non-NULL to be used to
   indicate transactions.  If two-way traffic is needed, e.g. to
   implement rpc, use two channels.  This makes the interface to async
   communication simpler as well.
*/

struct sm_channel {
    void *data;
};

/* System side.  Use these two to set up the begin-tick-end sequence. */
static inline void sm_push_begin(struct sm_channel *c, void *data) {
    c->data = data;
}
static inline int sm_push_end(struct sm_channel *c) {
    int accepted = !c->data;
    c->data = 0;
    return accepted;
}

/* Task side.  Use this e.g. in a SM_WAIT(s, (evt=sm_accept(s, &s->evt))) construct. */
static inline void *sm_read(struct sm_channel *c) {
    void *data = c->data;
    c->data = 0;
    return data;
}

/* Channel access inside a sm.  Note that it is also possible to
   implement CSP style select to wait on multiple transaction, but for
   now this seems to be enough. */
#define SM_READ(s, var, chan) \
    SM_WAIT(s, (var = sm_read(chan)))

#define SM_WRITE(s, var, chan) \
    ({ sm_push_begin(s, var); SM_WAIT(s, !((chan)->data)); })

#endif
