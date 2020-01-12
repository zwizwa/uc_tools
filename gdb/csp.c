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
#define ASSERT(x) if(!(x));
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










/* Scheduler loop

Each csp_schedule() call runs until all tasks are blocked.  For this
to be useful in practice there has to be some external influence that
creates new channel operations, opon which the scheduler can be used
to propagate through the network.

To simplify impelementation, the schedule loop does not assume any
outside influence: it knows only about tasks and channels.  Any event
coming from the outside is injected by emulating a task.  See
e.g. csp_send().

The general principle:

1. Precondition: everything is blocking.

2. Some external event happens (e.g. coming from a read or write
   interrupt on a microcontroller).  By some ad-hoc mechansim, this
   results in a (temporary) task are added to the hot list.

3. csp_schedule() loop: while there is a task on the hot list, find a
   corresponding blocked task on the cold list.

   3a.  There is no task.  Add the hot task to the cold list.

   3b.  There is a corresponding cold task.  Remove it from the cold
        list. Perform the data copy between writer and reader, resume
        both, and push their resulting suspension points to the hot
        list.

4. optionally: remove any temporary tasks.

*/


/* Revision:

   - there is one task list, and each task points to a select

   - every blocking action is a select.  send and receive are special
     cases of only one blocking action.

   - a select structure contains two lists: send list and receive
     list.  this is for easy pairing in the scheduler

   - each send and receive entry points to a channel and a data slot
     (value for send, cell for receive).

   I want to encode it efficiently because this is moving into
   pointerland.  Also static allocation is necessary, so it is ok to
   reserve space such that max num of slots is available.
*/


struct csp_op {
    uint16_t chan;
    uint16_t msg_len;
    void    *msg_buf;
};


struct csp_task {
    /* A task will be part of exactly one task list, so it's simplest
       to implement the pointers inside the task struct. */
    struct csp_task *next_task;

    /* After a channel op completes, the code is resumed through this
     * callback.  The internals of a task are opaque. */
    csp_resume resume;

    /* Each task blocks on a select operation, which can contain a
       mixture of sends and receives.  A select cotnains nb_send send
       ops and nb_receive receive ops.  There is always at least one
       op.  User needs allocate as many slots as are necessary
       throughout the program. */
    uint16_t nb_send;
    uint16_t nb_recv;
    struct csp_op op[];
};

/* The csp_op array has senders first, followed by receivers. */
static inline struct csp_op *task_op_send(struct csp_task *s) {
    return s->op;
}
static inline struct csp_op *task_op_recv(struct csp_task *s) {
    return &s->op[s->nb_send];
}

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
#define FOR_TASKS(pp,list) \
    for(struct csp_task **pp = &(list); *pp; pp = &((*pp)->next_task))


/* Task sets, implemented as stacks. */
struct csp_scheduler {
    // blocked tasks
    struct csp_task *cold;
    // suspended, need to be checked for rendez-vous
    struct csp_task *hot;
};

#define FOR_SENDOP(o, t) \
    for (struct csp_op *o = task_op_send(t); \
         o < (task_op_send(t) + t->nb_send); \
         o++)
#define FOR_RECVOP(o, t) \
    for (struct csp_op *o = task_op_recv(t); \
         o < (task_op_recv(t) + t->nb_recv); \
         o++)




/* Check if send can send to receive */
static inline int maybe_resume(
    struct csp_scheduler *sched,
    struct csp_task **cold_list_entry,
    struct csp_task *send,
    struct csp_task *recv) {

    /* Scan sender's send ops, and receiver's receive ops until there
     * is a match. */
    FOR_SENDOP(op_send, send) {

        FOR_RECVOP(op_recv, recv) {

            if (op_send->chan == op_recv->chan) {

                /* Rendez-vous found */

                /* Tasks can be removed from lists.  Hot list entry
                   has already been removed by caller.  Remove cold
                   list entry here.  This has to be done before adding
                   the task to another list.  We don't know if this is
                   the select or the send, so caller needs to pass in
                   this extra reference. */
                pop(cold_list_entry);

                /* Copy the data over the channel. */
                ASSERT(op_recv->msg_len <= op_send->msg_len);
                //LOG("memcpy %p %p %d\n", op_recv->msg_buf, op_send->msg_buf, op_send->msg_len);
                memcpy(op_recv->msg_buf, op_send->msg_buf, op_send->msg_len);
                op_recv->msg_len = op_send->msg_len;

                // FIXME: don't store in op.  Store it in task struct.
                // op_recv->type_chan = chan(op_send);

                /* Resume both, and push the resulting continuations
                   to the hot list for further evaluation.  NULL
                   resume means halt after this op.  Resume receiver
                   first.  That makes debug traces easier to read. */
                recv->resume(recv);
                if (recv->resume) {
                    push(&sched->hot, recv);
                }
                send->resume(send);
                if (send->resume) {
                    push(&sched->hot, send);
                }

                //LOG("!");
                return 1;
            }
        }
    }
    //LOG(".");
    return 0;
}

void csp_schedule(struct csp_scheduler *s) {
    struct csp_task *hot;

    /* Repeat until hot list is empty. */
  next_hot:
    if (!(hot = pop(&s->hot))) return;

    //LOG("s %p\n", s);
    FOR_TASKS(pcold, s->cold) {
        //LOG("pcold %p\n", pcold);
        struct csp_task *cold = *pcold;
        /* Check if these two tasks can rendez-vous */
        if (maybe_resume(s, pcold, hot, cold)) goto next_hot;
        if (maybe_resume(s, pcold, cold, hot)) goto next_hot;
    }
    push(&s->cold, hot);
    goto next_hot;
}




/* Start a task by letting it go through its initialization code.
   When resume returns, the task will either be blocking on send or
   receive, or halted. */
void csp_start(struct csp_scheduler *s, struct csp_task *t) {
    t->resume(t);
    if (t->resume) push(&s->hot, t);
    csp_schedule(s);
}


/* Halt is implemented as the absence of resume. */
static void csp_send_halt(struct csp_task *t) {
    t->resume = 0;
}
/* Synchronous send.  Note again that CSP is rendez-vous: there is no
   other place to put a value than inside a receiver's buffer.  If
   there is no receiver, there is no way to put it and this function
   will fail, as indicated by the return value. */
int csp_send(struct csp_scheduler *s,
             int chan,
             void *msg_buf,
             uint32_t msg_len) {
    struct {
        struct csp_task task;
        struct csp_op op;
    } t = {
        .task = {
            .resume = csp_send_halt,
            .nb_send = 1,
            .nb_recv = 0,
        },
        .op = {
            .chan = chan,
            .msg_len = msg_len,
            .msg_buf = msg_buf
        }
    };
    push(&s->hot, &t.task);
    csp_schedule(s);

    /* Post condition when we exit this function:
       - hot list will be empty, already so after csp_schedule()
       - cold list does not contain dangling reference to &t

       The latter we need to check, as it is possible that there was
       no select waiting on the channel, in which case we delete and
       signal failure to caller.
 */

    FOR_TASKS(pt, s->cold) {
        if (*pt == &t.task) {
            pop(pt);
            return 0;
        }
    }
    return 1;
}


/* E.g. a computed goto style state machine would have a pointer to
   store the resume point, and any other state data appended to the
   struct.  */
struct csp_task_sm {
    struct csp_task k; // header for scheduler
    void *next;        // next pointer for CG machine
    /* Other state vars. */
};

/* For use in computed goto machine.  See test code. */
static inline void csp_chanop1(struct csp_task *t,
                               void *msg_buf, uint32_t msg_len,
                               uint16_t chan,
                               uint16_t nb_send,
                               uint16_t nb_recv) {
    t->op[0].msg_buf = msg_buf;
    t->op[0].msg_len = msg_len;
    t->op[0].chan = chan;
    t->nb_send = nb_send;
    t->nb_recv = nb_recv;
}
#define CSP_CHANOP1(e,var,klabel,typch,ns,nr)                           \
    e->next = &&klabel;                                                 \
    csp_chanop1((struct csp_task *)(e),&(var),sizeof(var),typch,ns,nr); \
    return;                                                             \
klabel:

#define CSP_SEND(e,ch,var,klabel) \
    CSP_CHANOP1(e,var,klabel,ch,1,0)
#define CSP_RECV(e,ch,var,klabel) \
    CSP_CHANOP1(e,var,klabel,ch,0,1)




/* Buffered send.  This will succeed as long as the buffer doesn't
   overflow.  It requires a persistent task to monitor the buffer, and
   a transient task to "interrupt" the reader task.  This uses cbuf.
   Use SLIP to encode arbitrary objects. */
#ifdef CBUF_H
struct csp_buf {
    struct csp_task task;
    struct cbuf cbuf;
};
void csp_buf_task(struct csp_buf *b) {
    /* To make the interrupt work, we need to make sure we are always
     * waiting for it.  This requires synchronization on send AND
     * receive, which is currently not possible. */
    // FIXME
}

int csp_send_buffered(struct csp_scheduler *s,
                      struct csp_buf *b,
                      int reader_chan,
                      uint8_t *data, uint32_t len) {
    if (cbuf_room(&b->cbuf) < len) return 0;
    cbuf_write(&b->cbuf, data, len);

    /* Unblock the reader task. */
    ASSERT(1 == csp_send(s, reader_chan, &len, sizeof(len)));
    return 1;
}
#endif
