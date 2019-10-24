/* Plugin wrapper for forth.c

   Typically this is linked against a particular uC's Flash state.
   E.g. trace down build scripts for:

   redo forth.56ff6f065078525347130887.f103.bin
*/

#include "forth.c"


#include "plugin_api.h"

/* Loader calls this after reflashing. */
static void plugin_start(void) {
    plugin_init_memory();
    forth_start();
}
/* Header at start of .bin file */
struct plugin_service plugin PLUGIN_HEADER_SECTION = {
    .version = PLUGIN_API_VERSION,
    .io = { .read = forth_read, .write = forth_write },
    .start = plugin_start,
    .load_addr = &plugin
};

