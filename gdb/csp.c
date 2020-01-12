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

   - Support SEND and SELECT



   The basic structure of a CSP scheduler is straightforward.  The
   central idea is rendez-vous: both SEND and SELECT block, and get
   resumed when a compatible counterpart is waiting.

*/

#ifndef ASSERT
#define ASSERT(x)
#endif

#ifndef LOG
#define LOG(...)
#endif

#include <stdint.h>
#include <string.h>

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
    /* A task will be part of exactly one task list, so it's simplest
       to implement the pointers inside the task struct. */
    struct csp_task *next_task;

    csp_resume resume; // resume task, updating the continuation in place
    void *msg_buf;     // message being communicated in sender's memory
    uint32_t msg_size; // size of the message or buffer room for reception
    uint16_t op_type;  // type and nb_channels (can be >1 for select)
    uint16_t chan[1];  // channels (spills over to >1 for select)

    /* Channel spillover + private task data. */
};

/* Semantics of send: by the time execution resumes, the object has
   been delivered to the receiver, and the storage for the data
   element can be reclaimed for private use.  The reverse for select
   (receive): on resume, object has been written, and the k->chan
   contains the channel on which it was received.  Send is always on a
   single channel.  Receive (select) is 1 or more channels. */

/* The kind of operation is encoded in op_type before suspending.
   0x0000 is send with one channel
   Otherwise it is the number of channels in a select.

   Before resuming, op_type contains the channel ID that had the
   message.
*/

#define CSP_OP_SEND 0
#define CSP_OP_SELECT(n) n



/* E.g. a computed goto style state machine would have a pointer to
   store the resume point, and any other state data appended to the
   struct.  */
struct csp_task_sm {
    struct csp_task k; // header for scheduler
    void *next;        // next pointer for CG machine
    /* Other state vars. */
};

/* For use in computed goto machine.  See test code. */
#define CSP_CHANOP(e,ch,var,klabel,typ) \
    e->task.msg_buf = &(var);           \
    e->task.msg_size = sizeof(var);     \
    e->task.op_type = typ;              \
    e->task.chan[0] = ch;               \
    e->next = &&klabel;                 \
    return;                             \
klabel:

#define CSP_SEND(e,ch,var,klabel) CSP_CHANOP(e,ch,var,klabel,CSP_OP_SEND)
#define CSP_RECV(e,ch,var,klabel) CSP_CHANOP(e,ch,var,klabel,CSP_OP_SELECT(1))





/* Task sets, implemented as stacks. */
struct csp_scheduler {
    // blocked tasks
    struct csp_task *cold_send;
    struct csp_task *cold_select;
    // suspended, to check for block/resume
    struct csp_task *hot;
};

static inline struct csp_task *pop(struct csp_task **pstack) {
    struct csp_task *stack = *pstack;
    if (!stack) return 0;
    *pstack = stack->next_task;
    stack->next_task = 0; // POST: task is not in any list
    return stack;
}
static inline void push(struct csp_task **pstack, struct csp_task *task) {
    ASSERT(!task->next_task); // PRE: task is not in any list
    task->next_task = *pstack;
    *pstack = task;
}
/* Iterate over task list.  Uses double pointer for pop/push. */
#define FOR_TASKS(pb,list) \
    for(struct csp_task **pb = &(list); *pb; pb = &((*pb)->next_task))


/* Scheduler loop

Note that an external event buffer is not a real task, but we pretend
that a buffer for an input interrupt behaves as a task that has just
performed a write, and a buffer for an outgoing interrupt is a write
that does not cause any tasks to schedule and will only resume the
sender.

The data structures used to contain tasks are stacks.  This is ok
because we have no priority levels.


1. Precondition: everything is blocking.

2. Some external event happens (read or write interrupt).  The
   corresponding tasks are added to the hot list.

3. While there is a task on the hot list, find a corresponding blocked
   task on the cold list.

   3a.  There is no task.  Add the hot task to the cold list.

   3b.  There is a corresponding cold task.  Remove it from the cold
        list. Perform the data copy between writer and reader, resume
        both, and push their resulting suspension points to the hot
        list.
*/


/* Given a send and a select, check if they match.  If they do,
   execute the rendez-vous and remove the cold list entry. */

static inline int maybe_resume(
    struct csp_scheduler *s,
    struct csp_task **cold_list_entry,
    struct csp_task *select,
    struct csp_task *send) {

    ASSERT(select->op_type != CSP_OP_SEND);
    ASSERT(send->op_type == CSP_OP_SEND);

    int send_chan = send->chan[0];
    int n_chan = select->op_type;
    for (int i=0; i<n_chan; i++) {
        if (send_chan == select->chan[i]) {
            /* Rendez-vous found */

            /* Tasks can be removed from lists.  Hot list entry has
               already been removed by caller.  Remove cold list entry
               here.  This has to be done before adding the task to
               another list.  We don't know if this is the select or
               the send, so caller needs to pass in this extra
               reference. */
            pop(cold_list_entry);

            /* Copy the data over the channel. */
            ASSERT(select->msg_size <= send->msg_size);
            memcpy(select->msg_buf, send->msg_buf, send->msg_size);
            select->msg_size = send->msg_size;
            select->op_type = select->chan[i];

            /* Resume both, and push the resulting continuations to
               the hot list for further evaluation.  NULL resume means
               halt after this op. */
            select->resume(select);
            if (select->resume) {
                push(&s->hot, select);
            }
            send->resume(send);
            if (send->resume) {
                push(&s->hot, send);
            }

            LOG("!");
            return 1;
        }
        else {
            LOG(".");
        }
    }
    return 0;
}


void csp_schedule(struct csp_scheduler *s) {

    struct csp_task *a;

    /* Repeat until hot list is empty. */
  next_hot:
    if (!(a = pop(&s->hot))) return;

    /* Make the choice between send and receive early so we don't have
       to constantly check which op kind we're looking at.  This leads
       to two cases here with the inner routine shared. */
    if (a->op_type == CSP_OP_SEND) {
        FOR_TASKS(pb, s->cold_select) {
            struct csp_task *select = *pb, *send = a;
            if (maybe_resume(s, pb, select, send)) goto next_hot;
        }
        push(&s->cold_send, a);
    }
    else {
        FOR_TASKS(pb, s->cold_send) {
            struct csp_task *select = a, *send = *pb;
            if (maybe_resume(s, pb, select, send)) goto next_hot;
        }
        push(&s->cold_select, a);
    }
    goto next_hot;
}



void csp_start(struct csp_scheduler *s, struct csp_task *t) {
    t->resume(t);
    push(&s->hot, t);
    csp_schedule(s);
}




static void csp_send_halt(struct csp_task *t) {
    t->resume = 0;
}
int csp_send(struct csp_scheduler *s,
             int chan,
             void *msg_buf,
             uint32_t msg_len) {
    struct csp_task t = {
        .msg_buf = msg_buf,
        .msg_size = msg_len,
        .op_type = CSP_OP_SEND,
        .chan[0] = chan,
        .resume = csp_send_halt
    };
    push(&s->hot, &t);
    csp_schedule(s);

    /* Post condition when we exit this function:
       - hot list will be empty, already so after csp_schedule()
       - cold list does not contain dangling reference to &t

       The latter we need to check, as it is possible that there was
       no select waiting on the channel, in which case we delete and
       signal failure to caller.

       Note again that CSP is rendez-vous: there is no other place to
       put a value than inside a receiver's buffer.  If there is no
       receiver, there is no way to put it. */

    FOR_TASKS(pt, s->cold_send) {
        if (*pt == &t) {
            pop(pt);
            return 0;
        }
    }
    return 1;
}
