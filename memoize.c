#include "memoize.h"
#include "macros.h"
#include <string.h>

/* FIXME: this could be a generalized pbuf.h */
static inline int memoize_have(struct memoize_table *t, void *obj) {
    for(int i=0; i<t->nb_el; i++) {
        if (t->have[i] == obj) return 1;
    }
    return 0;
}
static inline void memoize_save(struct memoize_table *t, void *obj) {
    /* Precondition: !memoize_have() */
    /* FIXME: Can't do anything if the table is too small.  As
       graceful degradation we will result in re-evaluation. */
    if (t->max_nb_el == t->nb_el) {
        //LOG("ERROR: memoize overflow nb_el=%d\n", t->nb_el);
        return;
    }
    t->have[t->nb_el++] = obj;
}
/* Recursive evaluator. */
void memoize_eval(struct memoize_table *t,
                  void *obj,
                  memoize_eval_fn eval) {
    if (memoize_have(t, obj)) return;
    eval(t, obj);
    memoize_save(t, obj);
}
/* Top level evaluator. */
void memoize_eval_top(uintptr_t max_nb_el,
                      void *obj,
                      memoize_eval_fn eval) {
    const void *have[max_nb_el];
    memset(have, 0, sizeof(have[0]) * max_nb_el);
    struct memoize_table memoize_table = { .have = have, .max_nb_el = max_nb_el };
    memoize_eval(&memoize_table, obj, eval);
}
