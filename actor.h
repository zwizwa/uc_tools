/* As a counterweight to synchronous csp.c, also implement the actor
   model of asynchronous concurrency.  I currently have an application
   that is already set up like this in spirit, just not explicitly.

   In the uc_tools library, there are to abstractions for buffers:

   - cbuf is a circular character token buffer, i.e. a byte stream

   - pbuf is a flat packet buffer

   Both are useful in practice, mostly bridging the token and packet
   levels of abstraction.  The main property of pbuf is the flat
   memory layout.  The main property of cbuf is that it can be used as
   a lock free queue between interrupt and main loop on cortex M.


   To implement actors, the following abstractions are needed:

   - mailbox: a message queue

   - process: a state machine handling messages in a mailbox

   - scheduler: a program sequencing processes when they have messages
     in their mailboxes.


   The scheduler can be very simple: go over all processes in
   round-robin fashion and run them if they have data available.  My
   current application just polls the queue explicitly.  This way
   multiple queues can be supported.  I see no reason to change that
   right away.

   The main abstraction is the message queue.  Starting from Erlang as
   my actor model example, the main question to ask is should
   out-of-order handling be supported?  In my current application I
   don't need it, so let's skip it for now.

   To make things a bit more concrete: a process is a struct that
   contains two members: a cbuf token stream and a resume function.

*/

#ifndef ACTOR_H
#define ACTOR_H

#include "cbuf.h"

struct actor_task;
typedef uint32_t actor_status_t;
typedef actor_status_t (*actor_resume_f)(struct actor_task *k);

struct actor_task {
    struct cbuf *mbox;
    csp_resume_f resume;
    struct actor_task *next;
};
struct actor_scheduler {
    struct actor_task *tasks;
};

static inline uint32_t actor_send(struct actor_task *t, void *msg, uint32_t len) {
    return cbuf_write(t->mbox, msg, len);
}


#endif
