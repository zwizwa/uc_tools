#include "dataflow.h"

typedef struct dataflow_meta meta_t;
typedef struct dataflow_node node_t;
typedef union dataflow_value value_t;

/* Just use one operation. */
value_t add(const meta_t *m) {
    uintptr_t a = m->fwd_deps[0]->node->value.u;
    uintptr_t b = m->fwd_deps[1]->node->value.u;
    value_t v = {.u = a + b};
    return v;
}
void notify(const meta_t *m) {
    LOG("%p %d\n", m, m->node->value);
}

/* n1, n2: inputs */
extern const meta_t n3_meta;
node_t n1 = { };
const meta_t const* n1_fwd_deps[] = {NULL};
const meta_t const* n1_rev_deps[] = {&n3_meta, NULL};
const meta_t n1_meta = {
    .node = &n1,
    .notify = notify,
    .fwd_deps = n1_fwd_deps,
    .rev_deps = n1_rev_deps,
};

node_t n2 = { };
const meta_t const* n2_fwd_deps[] = {NULL};
const meta_t const* n2_rev_deps[] = {&n3_meta, NULL};
const meta_t n2_meta = {
    .node = &n2,
    .notify = notify,
    .fwd_deps = n2_fwd_deps,
    .rev_deps = n2_rev_deps,
};

/* n3: add */
node_t n3 = { };
extern const meta_t n5_meta;
const meta_t const* n3_fwd_deps[] = {&n1_meta, &n2_meta, NULL};
const meta_t const* n3_rev_deps[] = {&n5_meta, NULL};
const meta_t n3_meta = {
    .node = &n3,
    .update = add,
    .notify = notify,
    .fwd_deps = n3_fwd_deps,
    .rev_deps = n3_rev_deps,
};



/* n4: input */
node_t n4 = { };
const meta_t const* n4_fwd_deps[] = {NULL};
const meta_t const* n4_rev_deps[] = {&n5_meta, NULL};
const meta_t n4_meta = {
    .node = &n4,
    .notify = notify,
    .fwd_deps = n4_fwd_deps,
    .rev_deps = n4_rev_deps,
};

/* n5: add */
node_t n5 = { };
const meta_t const* n5_fwd_deps[] = {&n3_meta, &n4_meta, NULL};
const meta_t const* n5_rev_deps[] = {NULL};
const meta_t n5_meta = {
    .node = &n5,
    .update = add,
    .notify = notify,
    .fwd_deps = n5_fwd_deps,
    .rev_deps = n5_rev_deps,
};




int main(int argc, char **argv) {
    LOG("test_dataflow.c begin\n");
    LOG("test_dataflow.c end\n");

    value_t v;
    v.u = 1; dataflow_push(&n1_meta, v);
    v.u = 2; dataflow_push(&n2_meta, v);
    v.u = 4; dataflow_push(&n4_meta, v);
}
