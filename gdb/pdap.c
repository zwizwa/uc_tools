/* PDAP Parallel Debug Access Port interface */

/* TODO
   - first fix low level issues in blocking mode
   - integrate with actual forth
   - make it blocking, so there is backpressure by not handling usb (?)

   BUGS

   - compare-sections, mon reset, compare-sections seems to trigger
     bug in OpenOCD (if it's not still just mine).  mon reset, load,
     compare-sections works fine.  does it somehow not call run()
     after leaving some junk pointers in the queue?

   - probably my bug: there is a buffering issue


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
    // OpenOCD uses the synchronous commands only.
    //_service.add(app_poll);
    return 0;
}
DEF_INSTANCE(app);

