#ifndef SM_CSP_H
#define SM_CSP_H

/* Combining sm and csp mechanisms.

   Currently still in flux.  Attempting to find a set of abstractions
   to hide common patterns. */

#include "sm.h"
#include "csp.h"


#define SM_CSP_START(sched, task, name, state, ...) {           \
        name##_init((state), __VA_ARGS__);                      \
        (task)->resume = (csp_resume_f)name##_tick;             \
        csp_start((sched), (task));                             \
    }





#endif
