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

struct memoize_table;
typedef void (*memoize_eval_fn)(struct memoize_table *, void*);

/* Recursive evaluator. */
void memoize_eval(struct memoize_table *t, void *obj, memoize_eval_fn eval);

/* Top level evaluator. */
void memoize_eval_top(uintptr_t max_nb, void *obj, memoize_eval_fn eval);

#endif
