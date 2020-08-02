/* NOT TESTED */

#include "dataflow.h"

typedef struct dataflow_state state_t;
typedef struct dataflow_node node_t;

/* The "return d" is an ARM trick to keep the value in r0 to use less
   stack space. */

/* Differences with lua implementation is that eval does not return
   any values.  Everything is contained in the node structures. */
static state_t *eval(state_t *d, node_t *n) {
    /* Cache */
    if (n->valid) return d;
    /* Abort if it is an input node. */
    if (!n->meta) return d;
    /* Evaluate dependencies. */
    DATAFLOW_FOR_DEPS(n->meta->fwd_deps, fwd_dep) {
        d = eval(d, *fwd_dep);
        if (!(*fwd_dep)->valid) return d;
    }
    /* Update routine will set n->value. */
    n->meta->update(d, n);
    /* Marking is left to us. */
    n->valid = 1;
    return d;
}

/* Caller already has set the value. */
static state_t *propagate(state_t *d, node_t *n) {
    /* Propagate.  Invalidate all first, then evaluate. */
    DATAFLOW_FOR_DEPS(n->meta->rev_deps, rev_dep) {
        (*rev_dep)->valid = 0;
    }
    DATAFLOW_FOR_DEPS(n->meta->rev_deps, rev_dep) {
        d = eval(d, *rev_dep);
        d = propagate(d, *rev_dep);
    }
    return d;
}

void dataflow_push(state_t *d, node_t *n, union dataflow_value v) {
    n->valid = 1;
    n->value = v;
    propagate(d,n);
}



