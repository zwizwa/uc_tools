/* Configuration for hy1 board.
   Ad-hoc debugging board.

   The idea is to gather generic bits in mod_lab.c to have it work as
   an application framework, and define the app instance here.

*/

#define LEDSTRIP_NB_LEDS 32
#define PRODUCT "hy1"

#include "mod_lab.c"
#include "mod_ws2812.c"

instance_status_t app_init(instance_init_t *i) {
    INSTANCE_NEED(i, &console, &ledstrip);
    //_service.add(ledstrip_animation_tick);
    return 0;
}
DEF_INSTANCE(app);


struct grb frame[LEDSTRIP_NB_LEDS];

DEF_COMMAND(leds) { // r g b --
    struct grb grb;
    grb.b = command_stack_pop();
    grb.g = command_stack_pop();
    grb.r = command_stack_pop();
    for(int i=0; i<LEDSTRIP_NB_LEDS; i++) {
        frame[i] = grb;
    }
    ledstrip_send(frame);
}



int handle_tag_u32(const struct tag_u32 *s) {
    return -1;
}
