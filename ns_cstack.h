/* See other ns_*.h headers for the general idea.

   This module implements a simple circular stack.  Pushing a cstack
   that is full overwrites the oldest member.

   references:

   NS(_element_t), the stack base type.
   NS(_stack_t),   struct with stack, top, power-of-two mask

   Note that we are not tracking the actual size of the stack, so
   stack elements need to be able to distinguish between Some/None.

*/


#ifndef NS
#error define NS
#endif

#include <stdint.h>

/* Macros shared across multiple instances need to be guarded. */
#ifndef NS_STACK_H
#define NS_STACK_H
/* We assume the size is concrete. */
#define NS_STACK_SIZE(l) ARRAY_SIZE((l)->stack)
#endif



/* Precondition: l->top points to the next write slot. */
static inline void NS(_push)(NS(_stack_t) *l, NS(_element_t) e) {
    l->stack[l->top] = e;
    l->top = (l->top + 1) % NS_STACK_SIZE(l);
}
static inline uintptr_t NS(_size)(NS(_stack_t) *l) {
    return l->top;
}
static inline NS(_element_t) NS(_pop)(NS(_stack_t) *l) {
    l->top = (l->top - 1) % NS_STACK_SIZE(l);
    return l->stack[l->top];
}
static inline void NS(_init)(NS(_stack_t) *l) {
    l->top = 0;
}
/* 0=top, 1=2nd etc... */
static inline NS(_element_t)* NS(_index)(NS(_stack_t) *l, uintptr_t offset) {
    l->top = (l->top - offset - 1) % NS_STACK_SIZE(l);
    return &l->stack[l->top];
}


