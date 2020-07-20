#ifndef SM_CSP_H
#define SM_CSP_H

/* Combining sm and csp mechanisms.

   Currently still in flux.  Attempting to find a set of abstractions
   to hide common patterns. */

#include "sm.h"
#include "csp.h"


/* It seems simpler to set task.resume here, instead of in the
   name#_init function. */
#define SM_CSP_START(sched, name, state, ...) {                 \
        name##_init((state), __VA_ARGS__);                      \
        (state)->task.resume = (csp_resume_f)name##_tick;       \
        csp_start(sched, &((state)->task));                     \
    }

/* This needs some thought: two things need to be separated: access to
   the csp system (csp_task) and the resume mechanism of the task
   itself, which is opaque to the scheduler.  For now, provide an
   alternative implementation.  The only change seems to be to add an
   indirection. */

#define SM_CSP_SEL_K(state,_nb_send,_nb_recv,_klabel)      \
    (state)->task->nb_send = _nb_send;                     \
    (state)->task->nb_recv = _nb_recv;                     \
    (state)->next = &&_klabel;                             \
    return CSP_WAITING;                                    \
_klabel:

#define SM_CSP_SEL(state,ns,nr) \
    SM_CSP_SEL_K(state,ns,nr,GENSYM(resume_))




#endif
