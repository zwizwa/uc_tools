/* Configuration for bp2 board.
   Ad-hoc debugging board.

   The idea is to gather generic bits in mod_lab.c to have it work as
   an application framework, and define the app instance here.

*/

#define PRODUCT "bp2"
#define LEDSTRIP_NB_LEDS 32
#include "mod_lab.c"
#include "mod_console.c"
#include "cproc.h"


#define IN GPIOA,0

void app_poll(void) {
    static uint32_t app_timer;
    MS_PERIODIC(app_timer, 100) {
        uint32_t in = hw_gpio_read(IN);
        LET(in_edge,       /*=*/ edge, NULL, .in = in);
        LET(in_edge_count, /*=*/ acc,  NULL, .in = in_edge.out);
        if (in_edge.out) {
            infof("count = %d\n", in_edge_count);
        }
    }
}
instance_status_t app_init(instance_init_t *ctx) {
    INSTANCE_NEED(ctx, &console);
    _service.add(app_poll);
    return 0;
}
DEF_INSTANCE(app);

int handle_tag_u32(const struct tag_u32 *s) {
    return -1;
}


