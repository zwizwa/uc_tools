/* NOT TESTED */

#include "dataflow.h"

typedef struct dataflow_meta meta_t;
typedef struct dataflow_node node_t;
typedef union dataflow_value value_t;

void changed(const meta_t *n);

/* Returns 1 if propagation is necessary. */
static inline int set_val(const meta_t *m, value_t v) {
    node_t *n = m->node;
    n->valid = 1;
    if (n->initialized && (v.u == n->value.u)) return 0;
    n->initialized = 1;
    n->value = v;
    return 1;
}

/* Differences with lua implementation is that eval does not return
   any values.  Everything is contained in the node structures. */
void eval(const meta_t *m) {
    {
        node_t *n = m->node;
        /* Cache */
        if (n->valid) return;
        /* Abort if it is an input node. */
        if (!m->update) return;
    }
    /* Evaluate dependencies. */
    DATAFLOW_FOR_DEPS(m->fwd_deps, fwd_dep) {
        eval(*fwd_dep);
        if (!(*fwd_dep)->node->valid) return;
    }
    /* Update does not set the node value.  That is left up to us to
       stop propagation if value didn't change. */
    value_t v = m->update(m);
    if (!set_val(m, v)) return;
    changed(m);

}

/* Caller already has set the value. */
void changed(const meta_t *m) {
    /* External notification is registered explicitly, and should
       not be done as a side effect of the pure update routine. */
    if (m->notify) { m->notify(m); }

    /* It is essential that this is 2-pass: invalidate _all_ before
       evaluating them one by one. */
    DATAFLOW_FOR_DEPS(m->rev_deps, rev_dep) {
        (*rev_dep)->node->valid = 0;
    }
    DATAFLOW_FOR_DEPS(m->rev_deps, rev_dep) {
        eval(*rev_dep);
    }
}

void dataflow_push(const meta_t *m, value_t v) {
    if (!set_val(m, v)) return;
    changed(m);
}



