#ifndef SM_CSP_H
#define SM_CSP_H

/* Combining sm and csp mechanisms.

   Currently still in flux.  Attempting to find a set of abstractions
   to hide common patterns. */


/* It seems simpler to set task.resume here, instead of in the
   name#_init function. */
#define SM_CSP_START(sched, name, state, ...) {                 \
        name##_init((state), __VA_ARGS__);                      \
        (state)->task.resume = (csp_resume_f)name##_tick;       \
        csp_start(sched, &((state)->task));                     \
    }

#include "sm.h"
#include "csp.h"



#endif
