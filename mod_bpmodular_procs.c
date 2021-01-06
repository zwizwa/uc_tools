#ifndef MOD_BPMODULAR_PROCS
#define MOD_BPMODULAR_PROCS

/* FIXME: almost everything can be handled from the metastruct, except
 * for tick which instantiates consts. */

/* proc: in_A0 */

/* Going to side-step parameterization for now and specialize
   everything.  Focus on dataflow inputs. */
void tick_in_A0(struct inst *i) {
    infof("in_A0\n");
    const gpin_config c = { .port = GPIOA, .pin = 0 };
    gpin_update((gpin_state *)i->state, &c, NULL, NULL);
}
DEF_PROC_META(gpin);
const struct proc in_A0 = {
    .tick = tick_in_A0,
    .meta = &gpin_meta,
};
int apply_in_A0(struct tag_u32 *req) {
    return apply(req, &in_A0, 1, 0, NULL);
}

/* proc: edge */

DEF_PROC_META(edge);

void tick_edge(struct inst *inst) {
    uint32_t **pin = inputs(inst);
    edge_input in = { .in = *pin[0] };
    edge_update((edge_state *)inst->state, NULL, NULL, &in);
}
const struct proc edge = {
    .tick = tick_edge,
    .meta = &edge_meta,
};
int apply_edge(struct tag_u32 *req) {
    return apply(req, &edge, 1, 1, req->args);
}

/* proc: acc */

void tick_acc(struct inst *i) {
    infof("acc\n");
    acc_input in = { .in = inst_in(i, 0) };
    acc_update((acc_state *)i->state, NULL, NULL, &in);
}
DEF_PROC_META(acc);
const struct proc acc = {
    .tick = tick_acc,
    .meta = &acc_meta,
};
int apply_acc(struct tag_u32 *req) {
    return apply(req, &acc, 1, 1, req->args);
}




/* Metadata used in class handlers. */
const struct proc *proc[] = {&in_A0, &edge, &acc};


#endif
