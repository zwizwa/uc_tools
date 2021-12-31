/* Simple CSP scheduler
   See csp.c for more comments. */

#ifndef CSP_H
#define CSP_H

#include "cbuf.h"
#include "pbuf.h"
#include "gensym.h"

#include <stdint.h>
#include <string.h>

/* An event happens on a channel, and is associated with some data
   being transferred.  Direction is based on which array the event
   structure is stored in.

   Note that channel payload data is always copied to keep the
   semantics clean and simple: tasks do not share memory.

   Do not give into the temptation to share buffer memory between two
   tasks.  It is possible and not too hard to change the scheduler to
   support this, but it invalidates the no sharing guarnatee and will
   likely lead to subtle bugs.  The main advantage of the CSP approach
   is this kind of memory isolation.

   So, if shared memory or "memory moves" are to be implemented, then
   do it using a separate explicit mechanism.  E.g. a buffer pool that
   is taylored to the application, and pass buffer references around
   instead.
*/


/* See mod_csp_zerocopy.c for a specialized version. */
#ifndef CSP_CONF_COPY
#define CSP_CONF_COPY 1
#endif


union csp_msg {
    void     *v;
    uint8_t  *u8;
    uint16_t *u16;
    uint32_t *u32;
    /* Note that in some cases it might make sense to specialize the
       scheduler to word transfer only, where msg_len can be omitted
       as well. */
    uintptr_t w;
};
struct csp_evt {
    uint16_t chan;
#if CSP_CONF_COPY
    /* In zero-copy mode the object is abstract. */
    uint16_t msg_len;
#endif
    union csp_msg msg;
};


/* Initialize a single-channel operation. */
static inline void csp_evt(struct csp_evt *o,
                           uint16_t chan,
                           void *msg, uint32_t msg_len) {
    o->chan = chan;
    o->msg.v = msg;
#if CSP_CONF_COPY
    /* It's ok to leave msg_len in the argument list.
       Too much work to change the API. */
    o->msg_len = msg_len;
#endif
}


struct csp_task;
typedef uint32_t csp_status_t;
typedef csp_status_t (*csp_resume_f)(struct csp_task *k);

struct csp_task {
    /* A task will be part of exactly one task list, so it's simplest
       to implement the list pointers inside the task struct. */
    struct csp_task *next;

    /* After a channel op completes, the code is resumed through this
     * callback.  The internals of a task are opaque.  Set this to
     * NULL to indicate a halted task. */
    csp_resume_f resume;

    /* Each task blocks on a select operation, which can contain a
       mixture of sends and receives.  A select contains nb_send send
       ops and nb_receive receive ops.  There is always at least one
       op. */
    uint8_t nb_send;
    uint8_t nb_recv;
    uint8_t selected;
    uint8_t _res;

    /* Following this struct, user needs to allocate as many slots as
       are necessary throughout the program. */
    struct csp_evt evt[];
};

#define CSP_DIR_SEND 0
#define CSP_DIR_RECV 1

/* For use in computed goto machines. See example below.  Note that
   the scheduler doesn't care how a task is implemented.  It only
   knows about 'resume'.  However, we do need to provide a couple of
   tasks in the library, so the Computed Goto (CG) approach is used.

   Some assumptions to keep the macros simple, state struct has:
   - csp_task member named 'task'
   - next code pointer 'next' */

/* Select is the main blocking operation.  It waits for any of a set
   of channel operations (events) to complete.  Before calling
   CSP_SELECT, the corresponding csp_evt structures will need to be
   initialized. */

/* Note that the csp scheduler does not need a return value for the
   resume functions.  However, in order to be compatible with sm.h we
   do need to return a status code. */
#define CSP_HALTED  ((csp_status_t)0)
#define CSP_WAITING ((csp_status_t)0xFFFFFFFFUL)

/* Note: continuation and task need to be distiguished.  E.g. sm.h has
   composite continuations (e.g. a stack built out of nested structs),
   but has a single task struct to interface with csp scheduler.  The
   only thing we assume about the continuation is that it is
   implemented using computed goto, and that the continuation struct
   has a next field for the continuation address. */

#define CSP_SEL_K(task,cont,_nb_send,_nb_recv,_klabel)  \
    (task)->nb_send = _nb_send;                         \
    (task)->nb_recv = _nb_recv;                         \
    (cont)->next = &&_klabel;                           \
    return CSP_WAITING;                                 \
_klabel:

#define CSP_SEL(task,cont,ns,nr) \
    CSP_SEL_K(task,cont,ns,nr,GENSYM(resume_))

/* Data transfer event. */
#define CSP_EVT_BUF(task,n,ch,buf,size) \
    csp_evt(&((task)->evt[n]),ch,buf,size)
#define CSP_EVT(task,n,ch,var) \
    CSP_EVT_BUF(task,n,ch,&(var),sizeof(var))
/* Same, but no data payload (sync only).  Note that this just
   expresses intention, as shared data will be transferred in case
   sender is attaching a pointer. */
#define CSP_SYN(task,n,ch) \
    CSP_EVT_BUF(task,n,ch,0,0)
/* Hence this is exactly the same implementation. */
#define CSP_EVT_SHARED(task,n,ch) \
    CSP_EVT_BUF(task,n,ch,0,0)

/* Single op send and receive are special cases of select */
#define CSP_SND(task,cont,ch,var) {             \
        CSP_EVT(task,0,ch,var);                 \
        CSP_SEL(task,cont,1,0);                 \
}
#define CSP_RCV(task,cont,ch,var) {            \
        CSP_EVT(task,0,ch,var);                \
        CSP_SEL(task,cont,0,1);                \
}


/* This form is used by the .sm Scheme style language.  This uses
   zero-copy mode word transfer. */
#define CSP_RCV_W(task,cont,ch) ({              \
            CSP_EVT_BUF(task,0,ch,NULL,0);      \
            CSP_SEL(task,cont,0,1);             \
            (task)->evt[0].msg.w;               \
        })


/* RPC is a common pattern, so provide a macro for it. */
#define CSP_RPC(task,cont,ch,req_var,resp_var) {        \
        CSP_SND(task,cont,ch,req_var);                  \
        CSP_RCV(task,cont,ch,resp_var);                 \
    }

/* To halt a task: return from the resume function, but set a NULL
   resume point such that the task will not be rescheduled.  Note that
   the scheduler does not reclaim any resources. */
#define CSP_HALT(task) \
    {(task)->resume = 0; return;}




/* Scheduler data struct.  The hot list is the list of tasks to be
   checked against the cold list.  The cold list is the set of tasks
   currently blocked.  It is indexed by channel to allow fast pairing.
   Each channel maps directly to the event that is blocking a task.
   Multiple tasks can block on the same channel. */
struct csp_evt_list;
struct csp_evt_list {
    struct csp_evt_list *next;
    struct csp_task     *key;
    struct csp_evt      *evt;
};
struct csp_chan_to_evt {
    struct csp_evt_list *evts[2];
};
struct csp_scheduler {
    struct csp_task        *hot;
    struct csp_evt_list    *memory_pool;
    struct csp_chan_to_evt *chan_to_evt;
    uint16_t                nb_chans;
};

void csp_scheduler_init(
    struct csp_scheduler *s,
    struct csp_evt_list *c2e, int nb_c2e,
    struct csp_chan_to_evt *c, int nb_c);

void csp_with_scheduler(
    int nb_c2e, int nb_c,
    void (*f)(struct csp_scheduler *));


/* Run until all tasks are blocked. */
void csp_schedule(struct csp_scheduler *s);

/* Add a new task and schedule. */
void csp_start(struct csp_scheduler *s, struct csp_task *t);


#if 1
/* Example of a Computed Goto (CG) machine with one event buffer,
 * going through variable initialization and then sending out the
 * variable's value, alternating channel 1 and 2 */
struct csp_cg_example {
    struct csp_task task;
    struct csp_evt evt[1];
    void *next;
    uint32_t a;
};
static inline csp_status_t
csp_cg_example_resume(struct csp_cg_example *t /* task state */) {
    if (t->next) goto *t->next;
    t->a = 123;
    for(;;) {
        CSP_SND(&(t->task), t, 1, t->a);
        CSP_SND(&(t->task), t, 2, t->a);
    }
}
static inline void csp_cg_example_start(struct csp_scheduler *s, struct csp_cg_example *t) {
    memset(t,0,sizeof(*t));
    t->task.resume = (csp_resume_f)csp_cg_example_resume;
    csp_start(s, &t->task);
}
#endif





/* Send something to a channel from the outside of the CSP network.
   This can fail if there is no task waiting on the channel. */
int csp_send(struct csp_scheduler *s,
             int chan,
             void *msg_buf,
             uint32_t msg_len);




#ifdef CBUF_H
/* Contrasted with csp_send(), which can fail if there is no task
   waiting.  csp_async_send() will place data in a buffer as long as
   there is room.  A CSP task then handles transfer from the cbuf into
   the CSP network.  Dual for receive. */
struct csp_async {
    struct csp_task task;
    struct csp_evt evt[2]; // need to watch send and receive
    struct cbuf cbuf;
    void *next;
    uint16_t token;
    uint16_t nb_bytes;
    uint16_t c_int;
    uint16_t c_data;
};
/* The same object cam be started in read or write mode. */
void csp_async_write_start(struct csp_scheduler *s,
                           struct csp_async *b,
                           uint16_t c_int, uint16_t c_data,
                           void *buf, uint32_t size);
int csp_async_write(struct csp_scheduler *s,
                    struct csp_async *b,
                    const void *data, uint32_t len);

void csp_async_read_start(struct csp_scheduler *s,
                          struct csp_async *b,
                          uint16_t c_int, uint16_t c_data,
                          void *buf, uint32_t size);
int csp_async_read(struct csp_scheduler *s,
                   struct csp_async *b,
                   void *data, uint32_t len);

/* Notification: for an external writer, this signals the CSP network
   that it can read from the buffer.  */

void csp_async_notify(struct csp_scheduler*s, struct csp_async *b, uint16_t nb);

void csp_async_start(struct csp_scheduler *s,
                    struct csp_async *b,
                    csp_status_t (*task)(struct csp_async *),
                    uint16_t c_int, uint16_t c_data,
                    void *buf, uint32_t size);

csp_status_t csp_async_send_task(struct csp_async *b);


/* Simpler implementation using shared memory. */
int csp_async_write_shared(struct csp_scheduler *s,
                           uint16_t channel,
                           const void *data, uint32_t len);



#endif

#endif
