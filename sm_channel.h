#ifndef SM_EVENT_H
#define SM_EVENT_H

/* An interface to pass data to "poll-style" state machines, and an
   interpretation of the operation in a CSP setting (synchronous,
   non-buffered channels).

   Avoiding extra buffering is the core idea here.  That is why the
   Actor model (mailboxes, or buffered channels) is not useful.  We
   don't want to pay the memory overhead of dropping the data into a
   buffer.  We want to handle it when it is available and reclaim the
   storage immediately.



   But first: why use poll-style machines in the first place?  Why not
   build an entire application in CSP and have the correct semantics
   by design?

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

   Note that semantically, there is no real difference: events also
   represent conditions.  It is just that representation in C becomes
   very straightforward.  I.e. the line above would be equivalent to a
   blocking call:

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
static inline void *sm_accept(struct sm_channel *c) {
    void *data = c->data;
    c->data = 0;
    return data;
}

#endif
