#include "mod_plugin.c"

/* Loader calls this after reflashing. */
void plugin_start(void) {
    plugin_init_memory();
    infof("hello from plugin\n");
}
