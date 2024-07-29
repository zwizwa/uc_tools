#include "mod_plugin.c"

/* Loader calls this after reflashing. */
void plugin_start(void) {
    plugin_init_memory();
    infof("plugin_start: %s\n", __FILE__);
}
void plugin_stop(void) {
    infof("plugin_stop: %s\n", __FILE__);
}
