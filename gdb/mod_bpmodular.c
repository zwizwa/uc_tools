#ifndef MOD_BPMODULAR
#define MOD_BPMODULAR

/* A minimalistic modular synth for Blue Pill.

   Serves as a test application for patcher commands using the TAG_U32
   protocol.  Several simplifications are made:
   - all data and pointers are u32
   - use raw pointers in protocol to avoid indexing boiler plate

   Note that this is just a toy example.  Later, generate cproc.h
   processors and the glue code instantiation, which is hand-coded
   here.  Currently I'm convinced that cproc.h is a good API, but it's
   not the right abstraction level for writing a lot of processors
   manually.  Use a RAI-like or SM-like finer grained "naked state"
   macro substrate for that.
*/

/* FIXME:

   - each processor class should be a subdirectory with factory and
     metadata methods.

   - each instance should also be accessible via RPC.
*/

#include "hw_cproc_stm32f103.h"

/* The types describe what interface can be used to access information
   below this node. */
const char t_u32[] = "u32";
const char t_map[] = "map";
const char t_cmd[] = "cmd";
const char t_param[] = "param";

#define ALLOC_NB_WORDS 1024
struct alloc {
    uint32_t next;
    uint32_t buf[ALLOC_NB_WORDS];
} alloc;
typedef void (*tick_fn)(uint32_t *);

/* Processor metadata. */
struct inst;
struct proc {
    /* For access to inst over RPC, e.g. parameter get/set, dump */
    tag_u32_handle_fn handle;
    /* DSP tick method. */
    void (*tick)(struct inst *);
    uint32_t nb_state;
};

/* This is will be allocated in alloc.buf */
struct inst {
    uint32_t nb_words;
    const struct proc *proc;
    uint32_t state[];
};

struct inst *inst_first(void) {
    if (!alloc.next) return 0;
    return (void*)alloc.buf;
}
struct inst *inst_next(struct inst *i) {
    uint32_t index = ((uint32_t*)i) - alloc.buf;
    index += i->nb_words;
    if (index >= alloc.next) return NULL;
    return (void*)(&alloc.buf[index]);
}
#define FOR_INST(slice) \
    for(struct inst *i = inst_first(); i; i = inst_next(i))

/* External references are indices into alloc.buf, but they are not
   necessarily correct, so just scan the insts, as we don't have
   an inst index. */
struct inst *node_to_inst(uint32_t node) {
    struct inst *maybe_inst = (void*)&alloc.buf[node];
    FOR_INST(i) {
        if (maybe_inst == i) return i;
    }
    return 0;
}

/* This is ridiculously indirect. */
static inline uint32_t inst_in(struct inst *i, int in_nb) {
    uint32_t nb_state = i->proc->nb_state;
    uint32_t *in = (void*)i->state[nb_state + in_nb];
    return *in;
}


/* Run all DSP tick routines. */
void tick(void) {
    FOR_INST(i) {
        infof("inst %x\n", i);
        i->proc->tick(i);
    }
}

/* Aside from being used as dataflow identifiers, the node references
   can also be used to access arbitrary RPC functionality,
   e.g. parameter data. */
int handle_inst(struct tag_u32 *req) {
    if (req->nb_args < 1) {
        infof("bpmodular: handle_inst: missing arg\n");
        return -1;
    }
    uint32_t node = req->args[0];
    struct inst *inst = node_to_inst(node);
    if (!inst) {
        infof("bpmodular: %d is not a valid inst\n");
        return -1;
    }
    tag_u32_enter(req);
    int rv = inst->proc->handle(req);
    tag_u32_leave(req);
    return rv;
}

int reply_1(struct tag_u32 *req, uint32_t rv) {
    SEND_REPLY_TAG_U32(req, rv);
    return 0;
}

int apply(struct tag_u32 *req,
          const struct proc *proc,
          uint32_t n_state, uint32_t n_in, const uint32_t *in) {
    uint32_t n = 1 /* count */ + 1 /* tick */ + n_state + n_in;
    uint32_t node = alloc.next;
    struct inst *inst = (void*)&alloc.buf[node];
    uint32_t next = node + n;
    if (next > ALLOC_NB_WORDS) {
        infof("bpmodular: apply alloc failed\n");
        return -1;
    }
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
    // Commit
    inst->nb_words = n;  // Size of this record, for skipping to next.
    inst->proc = proc;   // Update routine
    alloc.next = next;
    // Use the pointer to the state struct as node identifier.
    return reply_1(req, node);
}



/* Generic handlers.  Note that if we keep the directory structure
   uniform, some handlers can be implemented by grabbing context from
   the higher up directories, i.e. indexing args with negative
   numbers.  FIXME: Some guards should be implemented for that. */

/* Parameter access.  This does not need metadata, as that can be
   gathered from the class handler. */
int handle_param(struct tag_u32 *req) {
    uint32_t node     = req->args[0];
    uint32_t param_nb = req->args[1];
    uint32_t cmd      = req->args[2];
    uint32_t *state   = &alloc.buf[node];  // FIXME: check bounds
    switch(cmd) {
    case 0: /* get */
        return reply_1(req, state[param_nb]);
    case 1: /* set */
        if (req->nb_args >= 4) {
            state[param_nb] = req->args[3];
        }
        return reply_1(req, 0);
    default:
        return -1;
    }
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
    };
    return HANDLE_TAG_U32_MAP(req, map);
}
const struct proc in_A0 = {
    .tick = tick_in_A0, .handle = handle_in_A0_inst, .nb_state = 1,
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
    .tick = tick_edge, .handle = handle_edge_inst, .nb_state = 1,
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
        {"count",t_param,1,handle_param},
    };
    return HANDLE_TAG_U32_MAP(req, map);
}
const struct proc acc = {
    .tick = tick_acc, .handle = handle_acc_inst, .nb_state = 1,
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
    alloc.next = 0;
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
