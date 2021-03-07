/* Configuration for bp3 board.
   Ad-hoc debugging board.

   The idea is to gather generic bits in mod_lab.c to have it work as
   an application framework, and define the app instance here.

*/

#define PRODUCT "bp3"
#include "mod_lab.c"
#include "mod_console.c"
#include "mod_map_forth.c"


//////////////////////////////////////////////////////////////////////

#include "mod_swd.c"

struct swd_serv swd_serv;
void app_poll(void) {
    swd_serv_tick(&swd_serv);
}

DEF_COMMAND(start) {
    swd_serv_init(&swd_serv);
}

instance_status_t app_init(instance_init_t *ctx) {
    infof("product: %s\n", PRODUCT);
    INSTANCE_NEED(ctx, &console, &swd);
    _service.add(app_poll);
    return 0;
}
DEF_INSTANCE(app);

int map_root(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        {"forth", "map", map_forth},
    };
    return HANDLE_TAG_U32_MAP(req, map);
}
int handle_tag_u32(struct tag_u32 *req) {
    int rv = map_root(req);
    if (rv) {
        infof("handle_tag_u32 returned %d\n", rv);
        /* Always send a reply when there is a from address. */
        send_reply_tag_u32_status_cstring(req, 1, "bad_ref");
    }
    return 0;
}


#if 1
KEEP uint32_t test_shift_right(uint32_t a, uint32_t b) {
    return a >> b;
}
KEEP uint32_t test_shift_left(uint32_t a, uint32_t b) {
    return a << b;
}
#endif
#if 1
struct ts {
    uint32_t top;
    uint32_t *stack;
};
KEEP struct ts test_add(struct ts ts) {
    ts.top += (*(ts.stack)++);
    return ts;
}
KEEP uint32_t *test_add_v2(uint32_t *stack) {
    stack[1] += stack[0];
    stack++;
    return stack;
}
#endif

KEEP void test_forth(void) {
    command_fun_start();
    command_fun_start();
}
