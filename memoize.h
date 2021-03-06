/* Call by need evaluator.
   FIXME: I need something quickly, without performance constraints.
   It's probably possible to do better than linear search.

   The API consists of two elements:

   - a recursive evaluator that is passed the memoization table

   - a toplevel evaluator that allocates the memoization table on the
     stack, and "pulls" the toplevel value.

   Note that this doesn't do any recursive value propagation.  You'll
   have to encode that in a wrapper.  This only performs "control
   dominates use" sequencing.

*/
#ifndef MEMOIZE_H
#define MEMOIZE_H

#include <stdint.h>

/* Expose the data structure, such that it can be used to derive a
   dependency-ordered list from a sequence of init calls. */
struct memoize_table {
    const void **have;
    uintptr_t nb_el;
    uintptr_t max_nb_el;
};

/* 0 == OK */
typedef uintptr_t memoize_status_t;
typedef memoize_status_t (*memoize_eval_fn)(struct memoize_table *, void*);

/* Recursive evaluator. */
memoize_status_t memoize_eval(struct memoize_table *t, void *obj, memoize_eval_fn eval);

/* Top level evaluator. */
memoize_status_t memoize_eval_top(uintptr_t max_nb, void *obj, memoize_eval_fn eval);

#endif
