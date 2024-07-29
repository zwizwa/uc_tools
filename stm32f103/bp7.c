/* Configuration for bp7 board.
   Ad-hoc debugging board.

   The idea is to gather generic bits in mod_lab.c to have it work as
   an application framework, and define the app instance here.

*/

#define PRODUCT "bp7"
#include "mod_lab.c"
#include "mod_console.c"
#include "mod_map_forth.c"


//////////////////////////////////////////////////////////////////////

#include "hw_swo.h"

void app_poll(void) {
    static uint32_t timer;
    static uint32_t count;
    MS_PERIODIC(timer, 10) {
        infof("count = %d\n", count++);
    }
}

instance_status_t app_init(instance_init_t *ctx) {
    infof("product: %s\n", PRODUCT);
    INSTANCE_NEED(ctx, &console);
    _service.add(app_poll);

    // nothing to see on SWO still, but probably SWD needs to be
    // enabled.  on boot the port is in jtag mode.

    // hw_swo_init_1();
    hw_swo_init_4();

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

