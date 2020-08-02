/* NOT TESTED */

#include "dataflow.h"


typedef struct dataflow_node node_t;
typedef union dataflow_value value_t;

void changed(node_t *n);

/* Returns 1 if propagation is necessary. */
static inline int set_val(node_t *n, value_t v) {
    n->valid = 1;
    if (n->initialized && (v.u == n->value.u)) return 0;
    n->initialized = 1;
    n->value = v;
    return 1;
}

/* Differences with lua implementation is that eval does not return
   any values.  Everything is contained in the node structures. */
void eval(node_t *n) {
    /* Cache */
    if (n->valid) return;
    /* Abort if it is an input node. */
    if (!n->meta) return;
    /* Evaluate dependencies. */
    DATAFLOW_FOR_DEPS(n->meta->fwd_deps, fwd_dep) {
        eval(*fwd_dep);
        if (!(*fwd_dep)->valid) return;
    }
    /* Update does not set the node value.  That is left up to us to
       stop propagation if value didn't change. */
    value_t v = n->meta->update(n);
    if (!set_val(n, v)) return;
    changed(n);

}

/* Caller already has set the value. */
void changed(node_t *n) {

    /* External notification is registered explicitly, and should not
       be done as a side effect of the pure update routine. */
    if (n->meta->notify) { n->meta->notify(n); }

    /* It is essential that this is 2-pass: invalidate _all_ before
       evaluating them one by one. */
    DATAFLOW_FOR_DEPS(n->meta->rev_deps, rev_dep) {
        (*rev_dep)->valid = 0;
    }
    DATAFLOW_FOR_DEPS(n->meta->rev_deps, rev_dep) {
        eval(*rev_dep);
    }
}

void dataflow_push(node_t *n, value_t v) {
    if (!set_val(n, v)) return;
    changed(n);
}



