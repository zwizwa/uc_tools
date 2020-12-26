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

/* mod_lab requires the handler to be defined. */
// bp2 ! {send_u32,[1,2,3]}.
int handle_tag_u32(
    void *context,
    const uint32_t *arg,  uint32_t nb_args,
    const uint8_t *bytes, uint32_t nb_bytes) {
    SEND_TAG_U32(4, nb_args);
    return 0;
}
