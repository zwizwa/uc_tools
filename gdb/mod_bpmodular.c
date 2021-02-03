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
    /* DSP tick method. */
    void (*tick)(struct inst *);
    const struct proc_meta *meta;
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
    uint32_t nb_state = i->proc->meta->state.nb_fields;
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
        send_reply_tag_u32_status_cstring(req, 1, "alloc_fail");
        return 0;
    }
    // Processor class info
    inst->proc = proc;
    // Init state.
    memset(inst->state, 0, n_state * sizeof(uint32_t));
    // Input connect
    for (int i=0; i<n_in; i++) {
        infof("input %d %d\n", i, in[i]);
        struct inst *in_inst = node_to_inst(in[i]);
        if (!in_inst) {
            send_reply_tag_u32_status_cstring(req, 1, "bad_node");
            balloci_drop(&alloc);
            return 0;
        }
        inst->state[n_state + i] = (uint32_t)in_inst->state;
    }
    // Use the pointer to the state struct as node identifier.
    return reply_1(req, node);
}

static inline uint32_t **inputs(struct inst *i) {
    const struct proc_meta *m = i->proc->meta;
    return (uint32_t**)&i->state[m->state.nb_fields];
}



int reply_bad_ref(struct tag_u32 *req) {
    infof("bpmodular: bad_ref\n");
    send_reply_tag_u32_status_cstring(req, 1, "bad_ref");
    return 0;
}

#define PARAM 0
#define STATE 1

/* Generic handlers.  Some notes:

   - If we keep the directory structure uniform, some handlers can be
     implemented by grabbing context from the higher up directories,
     i.e. indexing args with negative numbers.

   - It seems much better to explictly define methods with a map
     instead of handling them implicitly using MATCH clauses (see git
     history for handle_param).  That way there is metadata, and we
     also do not have to use pre-defined numeric identifiers.  A
     symbolic API can be used instead.
*/

/* Note that m->node and m->param_nb are only guarded upstream for
   directory access.  Anything else gets passed down here, where we
   need to do final validation.

   De map_ref metadata methods should return -1 for a bad reference:
   that is not an error.  However for direct path access, a bad
   reference is an error, and for that a generic handler reply_bad_ref() is
   used. */

int handle_param_get(struct tag_u32 *req) {
    TAG_U32_UNPACK(req, -4, m, node, kind, param_nb, cmd) {
        struct inst *i = node_to_inst(m->node);
        if (!i) return reply_bad_ref(req);
        infof("node=%d, kind=%d, nb=%d\n",
              m->node, m->kind, m->param_nb);
        if (PARAM == m->kind &&
            (m->param_nb < i->proc->meta->param.nb_fields)) {
            // FIXME: params are currently not stored!
            return reply_1(req, 0);
        }
        if (STATE == m->kind &&
            (m->param_nb < i->proc->meta->state.nb_fields)) {
            return reply_1(req, i->state[m->param_nb]);
        }
    }
    return reply_bad_ref(req);
}

int handle_param_set(struct tag_u32 *req) {
    TAG_U32_UNPACK(req, -4, m, node, kind, param_nb, cmd, val) {
        struct inst *i = node_to_inst(m->node);
        if (!i) return reply_bad_ref(req);
        infof("node=%d, kind=%d, nb=%d, val=%d\n",
              m->node, m->kind, m->param_nb, m->val);
        if (PARAM == m->kind &&
            (m->param_nb < i->proc->meta->param.nb_fields)) {
            // FIXME: params are currently not stored!
            return reply_1(req, 0);
        }
        if (STATE == m->kind &&
            (m->param_nb < i->proc->meta->state.nb_fields)) {
            i->state[m->param_nb] = m->val;
            return reply_1(req, 0);
        }
    }
    return reply_bad_ref(req);
}
int handle_param(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        {"get", t_cmd, handle_param_get},
        {"set", t_cmd, handle_param_set, 1},
    };
    return HANDLE_TAG_U32_MAP(req, map);
}

int map_param_entry(struct tag_u32 *req, void *ctx,
                    struct tag_u32_entry *entry) {
    TAG_U32_UNPACK(req, -2, m, node, kind, param_nb) {
        struct inst *i = node_to_inst(m->node);
        if (!i) return -1;
        const struct metastruct_struct *ms = NULL;
        switch(m->kind) {
        case PARAM: ms = &i->proc->meta->param; break;
        case STATE: ms = &i->proc->meta->state; break;
        default: return -1;
        }
        if (m->param_nb >= ms->nb_fields) return -1;
        const struct tag_u32_entry e = {
            .name = ms->fields[m->param_nb].name,
            .type = t_map,
        };
        *entry = e;
        return 0;
    }
    return -1;
}
int map_param(struct tag_u32 *req) {
    return handle_tag_u32_map_dynamic(req, handle_param, map_param_entry, NULL);
}

int map_inst_ops(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        [PARAM] = {"param", t_map, map_param},
        [STATE] = {"state", t_map, map_param},
    };
    return HANDLE_TAG_U32_MAP(req, map);
}

/* Aside from being used as dataflow identifiers, the node references
   can also be used to access arbitrary RPC functionality,
   e.g. parameter data. */

int map_inst_entry(struct tag_u32 *r, void *no_ctx,
                   struct tag_u32_entry *entry) {
    uint32_t index = r->args[0];
    struct inst *i = node_to_inst(index);
    if (!i) return -1;
    const struct tag_u32_entry e = {
        .name = 0, // unnamed, use node index instead
        .type = t_map
    };
    *entry = e;
    return 0;
}
int map_inst(struct tag_u32 *req) {
    return handle_tag_u32_map_dynamic(req, map_inst_ops, map_inst_entry, NULL);
}


#include "mod_bpmodular_procs.c"





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
int map_patch(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        {"reset", t_cmd, handle_reset},
        {"tick",  t_cmd, handle_tick},
    };
    return HANDLE_TAG_U32_MAP(req, map);
}



/* class ops map + handlers. */
int handle_apply(struct tag_u32 *req) {
    uint32_t index = req->args[-2];
    if (index >= ARRAY_SIZE(proc)) return reply_bad_ref(req);
    const struct proc *p = proc[index];
    if (req->nb_args != p->meta->input.nb_fields) return reply_bad_ref(req);
    infof("apply %s\n", p->meta->name);
    return apply(req, p,
                 p->meta->state.nb_fields,
                 p->meta->input.nb_fields,
                 req->args);
}
int handle_class_ops(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        {"apply", t_cmd, handle_apply}
    };
    return HANDLE_TAG_U32_MAP(req, map);
}

/* class map */
int map_class_entry(struct tag_u32 *r, void *_mr,
                    struct tag_u32_entry *entry) {
    uint32_t index = r->args[0];
    if (index >= ARRAY_SIZE(proc)) return -1;
    const struct proc *p = proc[index];
    const struct tag_u32_entry e = {
        .name = p->meta->name,
        .type = t_map,
        .handle = handle_class_ops
    };
    *entry = e;
    return 0;
}
int map_class(struct tag_u32 *req) {
    return handle_tag_u32_map_dynamic(req, handle_class_ops, map_class_entry, NULL);
}

/* root map */
int map_root(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        {"patch", t_map, map_patch},
        {"inst",  t_map, map_inst},
        {"class", t_map, map_class},
    };
    return HANDLE_TAG_U32_MAP(req, map);
}

/* Protocol handler entry point. */
int handle_tag_u32(struct tag_u32 *req) {
    int rv = map_root(req);
    if (rv) {
        /* Always send a reply when there is a from address. */
        send_reply_tag_u32_status_cstring(req, 1, "bad_ref");
    }
    return 0;
}

#endif
