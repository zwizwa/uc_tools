/* mod_i2c_tester.c: generic I2C tester code
   i2c_test.c: wrapper for specific blue pill board.
   See other mod_lab.c wrappers for more information.

*/

#define PRODUCT "i2c_test"
#include "mod_lab.c"
#include "mod_console.c"
#include "mod_map_forth.c"
#include "mod_i2c_tester.c"


void app_poll(void) {
}

instance_status_t app_init(instance_init_t *ctx) {
    infof("product: %s\n", PRODUCT);
    INSTANCE_NEED(ctx, &console);
    _service.add(app_poll);
    return 0;
}
DEF_INSTANCE(app);

int map_root(struct tag_u32 *req) {
    const struct tag_u32_entry map[] = {
        {"forth", "map", map_forth},
        {"i2c_tester", "map", map_i2c_tester},
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

