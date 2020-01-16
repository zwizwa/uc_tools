/* Stripped down C version of constrained parametric polymorphism
   (akin to type classes, ML functors, mixins).

   based on:
   - an NS prefixing macro to reference types and define names
   - reliance on field names in the C types (interface)

   references:
   NS(_t), a type with a ->next pointer to implement a singly linked list.

*/

/* FIXME:

   This is akin to Backus' functional forms vs. higher order
   functions.  Basically, the generic code is not first class, but
   more akin to a macro. */


#ifndef NS
#error define NS
#endif

#include <stdint.h>

/* O(1) */

/* Basic idea for all these header files is to not use memory
   allocation, so to keep it "linear", a list (stack) will need to be
   implemented in conjuction with a free list (also a stack).  This is
   why push and pop are defined in terms of list cells instead of list
   elements. */

static inline void NS(_push)(NS(_list_t) **l, NS(_list_t) *e) {
    ASSERT(!e->next); // PRE: element is not in any list
    e->next = *l;
    *l = e;
}
static inline NS(_list_t) *NS(_pop)(NS(_list_t) **l) {
    NS(_list_t) *e = *l;
    if (!e) return 0;
    *l = e->next;
    e->next = 0; // POST: element is not in any list
    return e;
}


