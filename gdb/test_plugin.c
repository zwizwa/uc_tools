#include "plugin_api.h"
#include "infof.h"

/* Loader calls this after reflashing. */
static void plugin_start(void) {
    plugin_init_memory();
    infof("hello from plugin\n");
}
/* Header at start of .bin file */
struct plugin_service plugin PLUGIN_HEADER_SECTION = {
    .version = PLUGIN_API_VERSION,
//  .io = { .read = plugin_read, .write = plugin_write },
    .start = plugin_start,
    .load_addr = &plugin,
    .endx_addr = &_eplugin
};

