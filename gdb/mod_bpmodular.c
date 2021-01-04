#ifndef MOD_BPMODULAR
#define MOD_BPMODULAR

/* A minimalistic modular synth for Blue Pill.

   Serves as a test application for patcher commands using the TAG_U32
   protocol.  Several simplifications are made:
   - all data and pointers are u32

   Note that this is just a toy example.  Later, generate cproc.h
   processors and the glue code instantiation, which is hand-coded
   here.  Currently I'm convinced that cproc.h is a good API, but it's
   not the right abstraction level for writing a lot of processors
   manually.  Use a RAI-like or SM-like finer grained "naked state"
   macro substrate for that.
*/

#include "hw_cproc_stm32f103.h"
#include "balloci.h"

/* The types describe what interface can be used to access information
   below this node. */
const char t_u32[] = "u32";
const char t_map[] = "map";
const char t_cmd[] = "cmd";
//const char t_param[] = "param";

#define ALLOC_NB_WORDS 1024
uint32_t alloc_buf[ALLOC_NB_WORDS];
struct balloci alloc = { .buf = alloc_buf, .size = ARRAY_SIZE(alloc_buf) };

typedef void (*tick_fn)(uint32_t *);

/* Processor metadata. */
struct inst;
struct proc {
    /* For access to inst over RPC, e.g. parameter get/set, dump */
    tag_u32_handle_fn handle;
    /* DSP tick method. */
    void (*tick)(struct inst *);
    uint32_t nb_state;
    const char *name;
};

/* This is will be allocated in alloc.buf */
struct inst {
    const struct proc *proc;
    uint32_t state[];
};


/* External references are indices into alloc.buf, but they are not
   necessarily correct, so just scan the insts, as we don't have
   an inst index.

   FIXME: This is currently used in the parameter setter, so should
   probably be replaced by a direct index mechanism.  A possible
   solution is to allocate instances at the top of the buffer, and
   allocate an instance index at the bottom.  This uses the same
   amount of memory because the size field can be removed.
*/
struct inst *node_to_inst(uint32_t node) {
    return (struct inst*)balloci_index(&alloc, node);
}

/* This is ridiculously indirect. */
static inline uint32_t inst_in(struct inst *i, int in_nb) {
    uint32_t nb_state = i->proc->nb_state;
    uint32_t *in = (void*)i->state[nb_state + in_nb];
    return *in;
}


/* Run all DSP tick routines. */
void tick(void) {
    for (uint32_t node = 0; node < alloc.count; node++) {
        struct inst *i = node_to_inst(node);
        infof("inst %x\n", i);
        i->proc->tick(i);
    }
}

int reply_1(struct tag_u32 *req, uint32_t rv) {
    SEND_REPLY_TAG_U32(req, rv);
    return 0;
}

int apply(struct tag_u32 *req,
          const struct proc *proc,
          uint32_t n_state, uint32_t n_in, const uint32_t *in) {
    uint32_t nb_words = 1 /* tick */ + n_state + n_in;
    uint32_t node = -1;
    struct inst *inst = (struct inst *)balloci_alloc(&alloc, nb_words, &node);
    if (!inst) {
        infof("bpmodular: alloc failed\n");
        return -1;
    }
    // Processor class info
    inst->proc = proc;
    // Init state.
    memset(inst->state, 0, n_state * sizeof(uint32_t));
    // Input connect
    for (int i=0; i<n_in; i++) {
        struct inst *in_inst = node_to_inst(in[i]);
        if (!in_inst) {
            infof("bp_modular: bad input node %d %d\n", i, in[i]);
            return -1;
        }
        inst->state[n_state + i] = (uint32_t)in_inst->state;
    }
    // Use the pointer to the state struct as node identifier.
    return reply_1(req, node);
}


/* Aside from being used as dataflow identifiers, the node references
   can also be used to access arbitrary RPC functionality,
   e.g. parameter data. */

struct inst_map_ref {
    char inst_name[8];
};
int inst_map_ref(struct inst_map_ref *mr, uint32_t index, struct tag_u32_entry *entry) {
    struct inst *i = node_to_inst(index);
    if (!i) return -1;
    // There is no sprintf... fix this.
    mr->inst_name[0] = 'i';
    mr->inst_name[1] = index + '0';
    mr->inst_name[2] = 0;
    const struct tag_u32_entry e = { .name = mr->inst_name, .type = i->proc->name };
    *entry = e;
    return 0;
}

/* 0:node 1:param 2:cmd 3... */
int handle_inst(struct tag_u32 *req) {
    if (req->nb_args < 1) {
        infof("bpmodular: handle_inst: missing arg\n");
        return -1;
    }
    if (req->args[0] == TAG_U32_CTRL) {
        /* Dynamically generated instance map. */
        struct inst_map_ref mr = {};
        return handle_tag_u32_map_ref_meta(req, (map_ref_fn)inst_map_ref, &mr);
    }
    uint32_t node = req->args[0];
    struct inst *inst = node_to_inst(node);
    if (!inst) {
        infof("bpmodular: bad node %d\n", node);
        return -1;
    }
    /* FIXME: Check parameter number as a guard for deeper behavior. */
    /* FIXME: Implement the map api. */

    /* Enter, so the param directory is cwd. */
    tag_u32_enter(req);
    int rv = inst->proc->handle(req);
    tag_u32_leave(req);
    return rv;
}

/* Generic handlers.  Some notes:

   - If we keep the directory structure uniform, some handlers can be
     implemented by grabbing context from the higher up directories,
     i.e. indexing args with negative numbers.

   - Parameter validation can be done up the stream.  E.g. we know
     that the param handlers all go through handle_inst(), which
     verifies the inst, so we can assume here that inst is correct.

   - It seems much better to explictly define methods with a map
     instead of handling them implicitly using MATCH clauses (see git
     history for handle_param).  That way there is metadata, and we
     also do not have to use pre-defined numeric identifiers.  A
     symbolic API can be used instead.
*/
int handle_param_get(struct tag_u32 *req) {
    uint32_t node     = req->args[-3];
    uint32_t param_nb = req->args[-2];
    struct inst *i    = node_to_inst(node);
    return reply_1(req, i->state[param_nb]);
}
int handle_param_set(struct tag_u32 *req) {
    uint32_t node     = req->args[-3];
    uint32_t param_nb = req->args[-2];
    uint32_t val      = req->args[0];
    struct inst *i    = node_to_inst(node);
    i->state[param_nb] = val;
    return reply_1(req, 0);
}
int handle_param(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        {"get", t_cmd,0,handle_param_get},
        {"set", t_cmd,0,handle_param_set},
    };
    return HANDLE_TAG_U32_MAP(req, map);
}


/* proc: in_A0 */

/* Going to side-step parameterization for now and specialize
   everything.  Focus on dataflow inputs. */
void tick_in_A0(struct inst *i) {
    infof("in_A0\n");
    const gpin_config c = { .port = GPIOA, .pin = 0 };
    gpin_update((gpin_state *)i->state, &c, NULL, NULL);
}
int handle_in_A0_inst(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        {"level",t_map,1,handle_param},
    };
    return HANDLE_TAG_U32_MAP(req, map);
}
const struct proc in_A0 = {
    .tick = tick_in_A0, .handle = handle_in_A0_inst, .nb_state = 1, .name = "in_A0"
};
int apply_in_A0(struct tag_u32 *req) {
    return apply(req, &in_A0, 1, 0, NULL);
}
int handle_in_A0_class(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        {"apply",t_cmd,0,apply_in_A0},
    };
    return HANDLE_TAG_U32_MAP(req, map);
}

/* proc: edge */


void tick_edge(struct inst *i) {
    infof("edge\n");
    edge_input in = { .in = inst_in(i, 0) };
    edge_update((edge_state *)i->state, NULL, NULL, &in);
}
int handle_edge_inst(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
    };
    return HANDLE_TAG_U32_MAP(req, map);
}
const struct proc edge = {
    .tick = tick_edge, .handle = handle_edge_inst, .nb_state = 1, .name = "edge"
};
int apply_edge(struct tag_u32 *req) {
    return apply(req, &edge, 1, 1, req->args);
}
int handle_edge_class(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        {"apply",t_cmd,0,apply_edge},
    };
    return HANDLE_TAG_U32_MAP(req, map);
}

/* proc: acc */

void tick_acc(struct inst *i) {
    infof("acc\n");
    acc_input in = { .in = inst_in(i, 0) };
    acc_update((acc_state *)i->state, NULL, NULL, &in);
}
int handle_acc_inst(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        {"count",t_map,1,handle_param},
    };
    return HANDLE_TAG_U32_MAP(req, map);
}
const struct proc acc = {
    .tick = tick_acc, .handle = handle_acc_inst, .nb_state = 1, .name = "acc"
};
int apply_acc(struct tag_u32 *req) {
    return apply(req, &acc, 1, 1, req->args);
}
int handle_acc_class(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        {"apply",t_cmd,0,apply_acc},
    };
    return HANDLE_TAG_U32_MAP(req, map);
}



/* Pacher commands. */

/* Individual node deletion is not supported.  We do not have malloc(). */
int handle_reset(struct tag_u32 *req) {
    balloci_clear(&alloc);
    SEND_REPLY_TAG_U32(req, 0);
    return 0;
}
/* Run network for a single tick. */
int handle_tick(struct tag_u32 *req) {
    tick();
    SEND_REPLY_TAG_U32(req, 0);
    return 0;
}

int handle_patch(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        {"reset", t_cmd,0,handle_reset},
        {"tick",  t_cmd,0,handle_tick},
        {"inst",  t_cmd,0,handle_inst},
    };
    return HANDLE_TAG_U32_MAP(req, map);
}
int handle_class(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        {"in_A0",t_map,0,handle_in_A0_class},
        {"edge", t_map,0,handle_edge_class},
        {"acc",  t_map,0,handle_acc_class},
    };
    return HANDLE_TAG_U32_MAP(req, map);
}
int handle_root(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        {"patch",t_map,-1,handle_patch},
        {"class",t_map,-1,handle_class},
    };
    return HANDLE_TAG_U32_MAP(req, map);
}

/* Protocol handler entry point. */
int handle_tag_u32(struct tag_u32 *req) {
    return handle_root(req);
}

#endif
