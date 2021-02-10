/* See other ns_*.h headers for the general idea.

   This module implements a simple stack.

   references:

   NS(_element_t), the stack base type.
   NS(_stack_t),   struct with stack, top

   There is no stack overflow detection mechanism.  The intention is
   to use this for an object pool index allocator.
*/


#ifndef NS
#error define NS
#endif

#include <stdint.h>

static inline void NS(_push)(NS(_stack_t) *l, NS(_element_t) e) {
    l->stack[l->top++] = e;
}
static inline uintptr_t NS(_size)(NS(_stack_t) *l) {
    return l->top;
}
static inline NS(_element_t) NS(_pop)(NS(_stack_t) *l) {
    return l->stack[--l->top];
}
static inline void NS(_init)(NS(_stack_t) *l) {
    l->top = 0;
}
static inline void NS(_pool_init)(NS(_stack_t) *l, uintptr_t n) {
    NS(_init)(l);
    while(n--) { NS(_push)(l, n); }
}


