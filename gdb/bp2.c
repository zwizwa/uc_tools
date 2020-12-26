/* Configuration for bp2 board.
   Ad-hoc debugging board.

   The idea is to gather generic bits in mod_lab.c to have it work as
   an application framework, and define the app instance here.

*/

#define PRODUCT "bp2"
#define LEDSTRIP_NB_LEDS 32
#include "mod_lab.c"
#include "mod_console.c"
instance_status_t app_init(instance_init_t *ctx) {
    INSTANCE_NEED(ctx, &console);
    SEND_TAG_U32(4,5,6);
    //_service.add(ledstrip_animation_tick);
    return 0;
}
DEF_INSTANCE(app);

