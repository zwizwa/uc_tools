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
    _service.add(app_poll);
    //_service.add(app_test);
    return 0;
}
DEF_INSTANCE(app);

/* These don't need arg checks: already guarded. */
int handle_a(struct tag_u32 *req) { SEND_REPLY_TAG_U32(req, req->args[0] + 1); return 0; }
int handle_b(struct tag_u32 *req) { SEND_REPLY_TAG_U32(req, req->args[0] - 1); return 0; }
int handle_d(struct tag_u32 *req) { SEND_REPLY_TAG_U32(req, req->args[0] + 1); return 0; }
int handle_e(struct tag_u32 *req) { SEND_REPLY_TAG_U32(req, req->args[0] - 1); return 0; }

int handle_sub(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        {"d","u32",1,handle_d},
        {"e","u32",1,handle_e},
    };
    return handle_tag_u32_map(req, map, ARRAY_SIZE(map));
}
int handle_root(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        {"a","u32",1,handle_a},
        {"b","u32",1,handle_b},
        {"c","map",-1,handle_sub}
    };
    return handle_tag_u32_map(req, map, ARRAY_SIZE(map));
}

int handle_tag_u32(struct tag_u32 *req) {
    return handle_root(req);
}









