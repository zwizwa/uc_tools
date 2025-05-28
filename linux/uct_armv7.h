#ifndef UCT_ARMV7_H
#define UCT_ARMV7_H
#include "macros.h"

/* To run thumb code on armv7 Cortex A it is necessary to perform a
   mode switch, which can be done by setting the low bit of the
   address to 1. */
#define CALL_THUMB(fun, ...) ({ \
    uintptr_t addr = ((uintptr_t)fun) | 1; \
    typeof(fun) *f = (void*)addr; \
    /* LOG("f=%p\n", f); */ \
    f(__VA_ARGS__); \
})

#endif
