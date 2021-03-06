/* Configuration for bp4 board.
   Ad-hoc debugging board.

   The idea is to gather generic bits in mod_lab.c to have it work as
   an application framework, and define the app instance here.

*/

#define PRODUCT "bp4"
#include "mod_lab.c"
#include "mod_console.c"
#include "mod_map_forth.c"

#include "mod_3level.c"

void app_poll(void) {
}

instance_status_t app_init(instance_init_t *ctx) {
    infof("product: %s\n", PRODUCT);
    INSTANCE_NEED(ctx, &console);
    _service.add(app_poll);

#ifdef MOD_3LEVEL
    init_3level();
#endif

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
