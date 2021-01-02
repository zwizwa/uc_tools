/* Configuration for hy1 board.
   Ad-hoc debugging board.

   The idea is to gather generic bits in mod_lab.c to have it work as
   an application framework, and define the app instance here.

*/

#define LEDSTRIP_NB_LEDS 32
#define PRODUCT "hy1"

#include "mod_lab.c"
#include "mod_ws2812.c"

instance_status_t app_init(instance_init_t *i) {
    INSTANCE_NEED(i, &console, &ledstrip);
    //_service.add(ledstrip_animation_tick);
    return 0;
}
DEF_INSTANCE(app);


struct grb frame[LEDSTRIP_NB_LEDS];

DEF_COMMAND(leds) { // r g b --
    struct grb grb;
    grb.b = command_stack_pop();
    grb.g = command_stack_pop();
    grb.r = command_stack_pop();
    for(int i=0; i<LEDSTRIP_NB_LEDS; i++) {
        frame[i] = grb;
    }
    ledstrip_send(frame);
}

/* Control tag is used for retrieving metadata for a particular node. */
#define TAG_U32_CTRL 0xFFFFFFFF

/* It has the following RPC requests defined as sub-tags: */
#define TAG_U32_CTRL_NB_NODES  0  /* Get nb of sub nodes at this node. */
#define TAG_U32_CTRL_NODE_ID   1  /* Get node id by node list index. */
#define TAG_U32_CTRL_ID_NAME   2  /* Map identifier to name */

/* Don't worry about consistency checks at this point.  Assume the
   host and communication channel can be trusted. */

#define TAG_U32_REPLY_U32(req, val) \
    infof("TAG_U32_REPLY_U32 %d\n", val)
#define TAG_U32_REPLY_CSTRING(req, str) \
    infof("TAG_U32_REPLY_U32 %s\n", str)

int handle_tag_u32(const struct tag_u32 *r) {
#if 1
    TAG_U32_MATCH_0(r, TAG_U32_CTRL) {
        const struct tag_u32 r1 = {
            .args = r->args+1, .nb_args = r->nb_args-1
        };
        /* Discovery protocol. */
        TAG_U32_MATCH_0(&r1, TAG_U32_CTRL_NB_NODES) {
            TAG_U32_REPLY_U32(r, command_index_size());
            return 0;
        }
        TAG_U32_MATCH(&r1, TAG_U32_CTRL_NODE_ID, m, index) {
            TAG_U32_REPLY_U32(r, m->index); // index is id
            return 0;
        }
        TAG_U32_MATCH(&r1, TAG_U32_CTRL_ID_NAME, m, id) {
            TAG_U32_REPLY_CSTRING(r, (*command_ref(m->id))->name);
            return 0;
        }
    }
#else
    if (r->nb_args >= 2 && r->args[0] == TAG_U32_CTRL) {
        switch(r->args[1]) {
        case TAG_U32_CTRL_NB_NODES:
            TAG_U32_REPLY_U32(r, command_index_size());
            return 0;
        case TAG_U32_CTRL_NODE_ID:
            TAG_U32_REPLY_U32(r, r->args[2]); // id
            return 0;
        case TAG_U32_CTRL_ID_NAME:
            TAG_U32_REPLY_CSTRING(r, (*command_ref(r->args[2]))->name);
            return 0;
        }
    }
#endif
    return -1;
}


