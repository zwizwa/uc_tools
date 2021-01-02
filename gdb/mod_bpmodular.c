#ifndef MOD_BPMODULAR
#define MOD_BPMODULAR

/* A minimalistic modular synth for Blue Pill.

   Serves as a test application for patcher commands using the TAG_U32
   protocol.  Several simplifications are made:
   - all data and pointers are u32
   - use raw pointers in protocol to avoid indexing boiler plate
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

void tick(void) {
    for(uint32_t *slice = alloc.buf; slice[0]; slice += slice[0]) {
        infof("slice %x\n", slice);
        tick_fn tick_ = (tick_fn)slice[1];
        tick_(&slice[2]);
    }
}

int apply(struct tag_u32 *req,
          tick_fn tick, uint32_t n_state, uint32_t n_in, const uint32_t *in) {
    uint32_t n = 1 /* count */ + 1 /* tick */ + n_state + n_in;
    // FIXME: Overrun is not checked.
    uint32_t *slice = alloc.buf + alloc.offset;
    uint32_t new_offset = alloc.offset + n;
    if (new_offset > ALLOC_NB_WORDS) return -1;
    alloc.offset = new_offset;
    slice[0] = n;              // Size of this record, for skipping to next.
    slice[1] = (uint32_t)tick; // Update routine
    // Input routing
    memset(slice + 1 + 1, 0, n_state * sizeof(uint32_t));
    if (n_in) memcpy(slice + 1 + 1 + n_state, in, n_in * sizeof(uint32_t));
    // Use the pointer to the state struct as node identifier.
    uint32_t node = (uint32_t)(slice+2);
    SEND_REPLY_TAG_U32(req, node); return 0;
    return 0;
}
/* Going to side-step parameterization for now and specialize
   everything.  Focus on dataflow inputs. */
void tick_in_A0(uint32_t *w) {
    infof("in_A0\n");
    const gpin_config c = { .port = GPIOA, .pin = 0 };
    gpin_update((gpin_state *)w, &c, NULL, NULL);
}
int apply_in_A0(struct tag_u32 *req) {
    return apply(req, tick_in_A0, 1, 0, NULL);
}

void tick_edge(uint32_t *w) {
    infof("edge\n");
    edge_input i = { .in = *w };
    edge_update((edge_state *)w, NULL, NULL, &i);
}
int apply_edge(struct tag_u32 *req) {
    return apply(req, tick_edge, 1, 1, req->args);
}

void tick_acc(uint32_t *w) {
    infof("acc\n");
    acc_input i = { .in = *w };
    acc_update((acc_state *)w, NULL, NULL, &i);
}
int apply_acc(struct tag_u32 *req) {
    return apply(req, tick_acc, 1, 1, req->args);
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
int handle_apply(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        {"in_A0",t_cmd,0,apply_in_A0},
        {"edge", t_cmd,0,apply_edge},
        {"acc",  t_cmd,0,apply_acc},
    };
    return HANDLE_TAG_U32_MAP(req, map);
}
int handle_root(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        {"patch",t_map,-1,handle_patch},
        {"apply",t_map,-1,handle_apply}
    };
    return HANDLE_TAG_U32_MAP(req, map);
}

/* Protocol handler entry point. */
int handle_tag_u32(struct tag_u32 *req) {
    return handle_root(req);
}

#endif
