/* PDAP Parallel Debug Access Port interface */

/* TODO
   - make this console only.  for tag_u32 access, see bp3.c
*/

#define PRODUCT "pdap"
#include "mod_standalone.c"
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

