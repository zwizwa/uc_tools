/* PDAP Parallel Debug Access Port interface */

/* TODO
   - make this console only.  for tag_u32 access, see bp3.c
   - set up build system to easily develop applications like this
   - integrate with actual forth
   - change interpreter to do number interpretation first
   - make it blocking, so there is backpressure by not handling usb
*/

#define PRODUCT "pdap"
#include "mod_standalone.c"
#include "mod_swd.c"

struct swd_serv swd_serv;
void app_poll(void) {
    swd_serv_tick(&swd_serv);
}

instance_status_t app_init(instance_init_t *ctx) {
    infof("product: %s\n", PRODUCT);
    INSTANCE_NEED(ctx, &console, &swd);
    _service.add(app_poll);
    return 0;
}
DEF_INSTANCE(app);

