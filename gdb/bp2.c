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
        PROC(in_edge,       /*=*/ edge, NULL, NULL, .in = in);
        PROC(in_edge_count, /*=*/ acc,  NULL, NULL, .in = in_edge.out);
        if (in_edge.out) {
            infof("count = %d\n", in_edge_count);
        }
    }
}
void app_test(void) {
    static uint32_t test_timer;
    MS_PERIODIC(test_timer, 1000) {
        infof("tick...\n");
    }
}

instance_status_t app_init(instance_init_t *ctx) {
    INSTANCE_NEED(ctx, &console);
    //_service.add(app_poll);
    //_service.add(app_test);
    return 0;
}
DEF_INSTANCE(app);

#define REPLY_LOG(...) infof(__VA_ARGS__)

#define REPLY_U32(req, val) {                   \
        REPLY_LOG("REPLY_U32 %d\n", val);       \
        SEND_REPLY_TAG_U32(req, val);           \
        return 0;                               \
    }

#define REPLY_CSTRING(req, str) {                     \
        REPLY_LOG("REPLY_U32 %s\n", str);             \
        SEND_REPLY_TAG_U32_CSTRING(req, str);         \
        return 0;                                     \
    }
#define CASE_0 TAG_U32_MATCH_0
#define CASE TAG_U32_MATCH

#define SYM(tag) SYMBOL_INDEX(tag)

#define CASE_CMD_0(r,cmd) CASE_0(r,command_index(&cmd))

DEF_SYMBOL(abc);
DEF_SYMBOL(def);

/* Two modifications are necessary:

   - The request "pointer" should be writeable, so we can push/pop
     when decending into substructure.

   - Each level should be discoverable.
*/





/* r->nb_args is already guarded before calling these. */
int handle_abc(struct tag_u32 *r) { REPLY_U32(r, r->args[0] + 1); }
int handle_def(struct tag_u32 *r) { REPLY_U32(r, r->args[0] - 1); }

int handle_tag_u32(struct tag_u32 *r) {
    const struct tag_u32_entry map[] = {
        {"abc","u32",1,handle_abc},
        {"def","u32",1,handle_def},
    };
    return handle_tag_u32_map(r, map, ARRAY_SIZE(map));
}





#if 0
int handle_tag_u32(struct tag_u32 *r) {
    const char *tags[] = {"abc","def"};
    CASE(r, 0, m, val) {
        REPLY_U32(r, m->val+1);
    }
    CASE(r, 1, m, val) {
        REPLY_U32(r, m->val-1);
    }
    CASE_0(r, TAG_U32_CTRL) {
        tag_u32_enter(r);
        CASE(r, TAG_U32_CTRL_ID_NAME, m, id) {
            REPLY_CSTRING_ARR(r, tags, m->id);
        }
        tag_u32_leave(r);
    }
    return -1;
}
#endif

#if 0
/* FIXME: It's probably simpler to dump the symbol table by letting it
   terminate on an empty string.  Only one call is necessary in that
   case, but the tradeoff is that such a thing cannot be requested
   without pingpong. */

/* For actual use, use dedicated symbols. */
int handle_tag_u32(struct tag_u32 *r) {

    CASE_0(r, SYM(abc)) {
        REPLY_U32(r, 123);
    }
    CASE_0(r, SYM(def)) {
        REPLY_U32(r, 456);
    }

    CASE_0(r, TAG_U32_CTRL) {
        /* FIXME: create a "with" macro that descends into the tree by
         * in-place updating of the input struct. */
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
#endif


/* Exploratory: command dictionary. */

#if 0
int handle_tag_u32(struct tag_u32 *r) {
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




