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
    infof("product: %s\n", PRODUCT);
    INSTANCE_NEED(ctx, &console);
    _service.add(app_poll);
    //_service.add(app_test);
    return 0;
}
DEF_INSTANCE(app);

#include "mod_bpmodular.c"

#if 0
/* These don't need arg checks: already guarded. */
int handle_a(struct tag_u32 *req) { SEND_REPLY_TAG_U32(req, req->args[0] + 1); return 0; }
int handle_b(struct tag_u32 *req) { SEND_REPLY_TAG_U32(req, req->args[0] - 1); return 0; }
int handle_d(struct tag_u32 *req) { SEND_REPLY_TAG_U32(req, req->args[0] + 1); return 0; }
int handle_e(struct tag_u32 *req) { SEND_REPLY_TAG_U32(req, req->args[0] - 1); return 0; }

const char t_u32[] = "u32";
const char t_map[] = "map";


int handle_sub(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        {"d",t_u32,1,handle_d},
        {"e",t_u32,1,handle_e},
    };
    return HANDLE_TAG_U32_MAP(req, map);
}
int handle_root(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        {"a",t_u32,1,handle_a},
        {"b",t_u32,1,handle_b},
        {"c",t_map,-1,handle_sub}
    };
    return HANDLE_TAG_U32_MAP(req, map);
}

int handle_tag_u32(struct tag_u32 *req) {
    int rv = handle_root(req);
    if (rv) {
        infof("bp2: handle_tag_u32 returned %d\n", rv);
        /* Always send a reply when there is a from address. */
        send_reply_tag_u32_status_cstring(req, 1, "bad_ref");
    }
    return 0;
}
#endif








