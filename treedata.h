#ifndef TREEDATA_H
#define TREEDATA_H

/* Self-describing tree data.

   Use in conjunction with the double-buffered mmap-based arena
   allocator to copy data structure from one arena into another.

   Basic idea:
   - Pointers
   - Tagged integers representing other types

   Assuming 32-bit pointers, 32-bit aligned. There are 2 tag bits
   available giving 4 types:
   - pointer   (internal or external)
   - integer
   - reserved2
   - reserved3

   On 64-bit there are 2 more tag bits.  Not using those atm.

   I do want the pointers to be straightforward C pointers.  The
   tagged integers will need "accessors" and could be typed as
   pointers to have some more native C type checking.

   I want to represent external pointers also as straightforward C
   pointers.  The allocator that will do the copying be able to
   distinguish them simply by checking if they are inside the arena or
   not.

   EDIT: Orignal plan was to put this in the arena allocator, but I am
   going to split it up.  The arena allocator will need to not put any
   constraints on the C data structures.  If copying is needed,
   explicit C copy constructors will need to be used.  The treedata
   representation makes more sense when building a scripting language
   on top of of the arenas allocator to then build a copying GC.

*/

#include "macros.h"

#if !defined(__GNUC__)
#error "Code uses integer-as-pointer representation. Ok on GCC and Clang but not sure where else."
#endif

typedef struct {} td_int_t;

#define TD_TAG_INT   1
#define TD_TAG_SHIFT 2
#define TD_TAG_MASK  ((1 << TD_TAG_SHIFT)-1)

static inline intptr_t td_get_int(td_int_t *x) {
    uintptr_t i = (uintptr_t)x;
    ASSERT(TD_TAG_INT == (i & TD_TAG_MASK));
    return i >> TD_TAG_SHIFT;
}

static inline td_int_t *td_make_int(intptr_t i) {
    uintptr_t tagged = (i << TD_TAG_SHIFT) + TD_TAG_INT;
    return (td_int_t*)tagged;
}


#endif
