/* Configuration for bp2 board.
   Ad-hoc debugging board.

   The idea is to gather generic bits in mod_lab.c to have it work as
   an application framework, and define the app instance here.

*/

#define PRODUCT "bp2"
#define LEDSTRIP_NB_LEDS 32
#include "mod_lab.c"
#include "mod_console.c"
#include "cproc.h"

#include "symbol.h"

#define IN GPIOA,0

void app_poll(void) {
    static uint32_t app_timer;
    MS_PERIODIC(app_timer, 100) {
        uint32_t in = hw_gpio_read(IN);
        LET(in_edge,       /*=*/ edge, NULL, .in = in);
        LET(in_edge_count, /*=*/ acc,  NULL, .in = in_edge.out);
        if (in_edge.out) {
            infof("count = %d\n", in_edge_count);
        }
    }
}
instance_status_t app_init(instance_init_t *ctx) {
    INSTANCE_NEED(ctx, &console);
    _service.add(app_poll);
    return 0;
}
DEF_INSTANCE(app);


#define REPLY_U32(req, val) {                   \
        /* infof("REPLY_U32 %d\n", val); */     \
        SEND_REPLY_TAG_U32(req, val);           \
        return 0;                               \
    }

#define REPLY_CSTRING(req, str) {               \
        /* infof("REPLY_U32 %s\n", str); */     \
        SEND_REPLY_TAG_U32_CSTRING(req, str);        \
        return 0;                               \
    }
#define CASE_0 TAG_U32_MATCH_0
#define CASE TAG_U32_MATCH

#define SYM(tag) SYMBOL_INDEX(tag)

#define CASE_CMD_0(r,cmd) CASE_0(r,command_index(&cmd))

DEF_SYMBOL(abc);
DEF_SYMBOL(def);

/* For actual use, use dedicated symbols. */
int handle_tag_u32(const struct tag_u32 *r) {
    CASE_0(r, SYM(abc)) {
        REPLY_U32(r, 123);
    }
    CASE_0(r, SYM(def)) {
        REPLY_U32(r, 456);
    }

    CASE_0(r, TAG_U32_CTRL) {
        const struct tag_u32 r1 = TAG_U32_SHIFT(r, 1);
        /* Discovery protocol. */
        CASE_0(&r1, TAG_U32_CTRL_NB_NODES) {
            REPLY_U32(r, symbol_index_size());
        }
        CASE(&r1, TAG_U32_CTRL_NODE_ID, m, index) {
            if (m->index < symbol_index_size()) {
                REPLY_U32(r, m->index); // index is id
            }
        }
        CASE(&r1, TAG_U32_CTRL_ID_NAME, m, id) {
            REPLY_CSTRING(r, *symbol_ref(m->id));
        }
    }
    return -1;
}


/* Exploratory: command dictionary. */

#if 0
int handle_tag_u32(const struct tag_u32 *r) {
    //CASE_CMD_0(r, commands) {
    //}

    CASE_0(r, TAG_U32_CTRL) {
        const struct tag_u32 r1 = TAG_U32_SHIFT(r, 1);
        /* Discovery protocol. */
        CASE_0(&r1, TAG_U32_CTRL_NB_NODES) {
            REPLY_U32(r, command_index_size());
        }
        CASE(&r1, TAG_U32_CTRL_NODE_ID, m, index) {
            if (m->index < command_index_size()) {
                REPLY_U32(r, m->index); // index is id
            }
        }
        CASE(&r1, TAG_U32_CTRL_ID_NAME, m, id) {
            REPLY_CSTRING(r, (*command_ref(m->id))->name);
        }
    }
    return -1;
}
#endif




