#ifndef MOD_POLL_PLUGIN
#define MOD_POLL_PLUGIN

/* A plugin with a poll loop and some safety features to deal with
   crashes. */

#include "tag_u32.h"
int plugin_tag_u32(struct tag_u32 *);
#define HANDLE_TAG_U32 plugin_tag_u32
#include "mod_plugin.c"
#include "cycle_counter.h"

#include <libopencm3/stm32/f1/iwdg.h>

#define MS_PERIODIC(var, ms) \
    CYCLE_COUNTER_PERIODIC(var, ((ms) * 72000))

void plugin_poll(void);
void plugin_init(void);

void plugin_watchdog(void) {
    iwdg_reset();
}

/* Loader calls this after reflashing. */
void plugin_start(void) {
    plugin_init_memory();
    enable_cycle_counter();
    //infof("plugin_start: %s\n", __FILE__);
    iwdg_set_period_ms(1000);
    iwdg_start();
    plugin_init();
    _service.add(plugin_watchdog);
    _service.add(plugin_poll);
}
void plugin_stop(void) {
    //infof("plugin_stop: %s\n", __FILE__);
    _service.reset();

    /* FIXME: Currently the watchdog is not turned off.  This means
       that you cannot load another plugin that doesn't use the
       watchdog.  This seems to be ok as the plugin reloads within the
       watchdog time. */
}

#endif
