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

const char t_u32[] = "u32";
const char t_map[] = "map";
const char t_cmd[] = "cmd";

#define ALLOC_NB_WORDS 1024
struct alloc {
    uint32_t offset;
    uint32_t buf[ALLOC_NB_WORDS];
} alloc;
typedef void (*tick_fn)(uint32_t *);

struct proc {
    /* For access to instance over RPC, e.g. parameter get/set, dump */
    tag_u32_handle_fn handle;
    /* DSP tick method. */
    void (*tick)(uint32_t *w);
};

/* Run all DSP tick routines. */
void tick(void) {
    for(uint32_t *slice = alloc.buf; slice[0]; slice += slice[0]) {
        infof("slice %x\n", slice);
        const struct proc *proc = (void*)slice[1];
        proc->tick(&slice[2]);
    }
}

/* Aside from being used as dataflow identifiers, the node references
   can also be used to access arbitrary RPC functionality,
   e.g. parameter data. */
int handle_instance(struct tag_u32 *req, uint32_t node) {
    // FIXME: check bounds
    const struct proc *proc = (void*)alloc.buf[node-1];
    return proc->handle(req);
}

int apply(struct tag_u32 *req,
          const struct proc *proc,
          uint32_t n_state, uint32_t n_in, const uint32_t *in) {
    uint32_t node = -1;  // invalid
    uint32_t n = 1 /* count */ + 1 /* tick */ + n_state + n_in;
    uint32_t *slice = alloc.buf + alloc.offset;
    uint32_t new_offset = alloc.offset + n;
    if (new_offset > ALLOC_NB_WORDS) return -1;
    slice[0] = n;              // Size of this record, for skipping to next.
    slice[1] = (uint32_t)proc; // Update routine
    // Init state.
    memset(slice + 1 + 1, 0, n_state * sizeof(uint32_t));
    // Input routing
    for (int i=0; i<n_in; i++) {
        if (in[i] >= ALLOC_NB_WORDS) goto abort;
        slice[1 + 1 + n_state + i] = alloc.buf[in[i]];
    }
    // Commit
    alloc.offset = new_offset;
    // Use the pointer to the state struct as node identifier.
    node = (uint32_t)(slice + 2 - alloc.buf);
  abort:
    SEND_REPLY_TAG_U32(req, node); return 0;
    return 0;
}
/* proc: tick */

/* Going to side-step parameterization for now and specialize
   everything.  Focus on dataflow inputs. */
void tick_in_A0(uint32_t *w) {
    infof("in_A0\n");
    const gpin_config c = { .port = GPIOA, .pin = 0 };
    gpin_update((gpin_state *)w, &c, NULL, NULL);
}
int handle_in_A0_instance(struct tag_u32 *req) {
    return -1;
}
const struct proc in_A0 = {
    .tick = tick_in_A0, .handle = handle_in_A0_instance
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

void tick_edge(uint32_t *w) {
    infof("edge\n");
    edge_input i = { .in = *w };
    edge_update((edge_state *)w, NULL, NULL, &i);
}
int handle_edge_instance(struct tag_u32 *req) {
    return -1;
}
const struct proc edge = {
    .tick = tick_edge, .handle = handle_edge_instance
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

void tick_acc(uint32_t *w) {
    infof("acc\n");
    acc_input i = { .in = *w };
    acc_update((acc_state *)w, NULL, NULL, &i);
}
int handle_acc_instance(struct tag_u32 *req) {
    return -1;
}
const struct proc acc = {
    .tick = tick_acc, .handle = handle_acc_instance
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
    alloc.offset = 0;
    alloc.buf[0] = 0;
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
        {"reset",t_cmd,0,handle_reset},
        {"tick", t_cmd,0,handle_tick},
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
        {"class",t_map,-1,handle_class}
    };
    return HANDLE_TAG_U32_MAP(req, map);
}

/* Protocol handler entry point. */
int handle_tag_u32(struct tag_u32 *req) {
    return handle_root(req);
}

#endif
