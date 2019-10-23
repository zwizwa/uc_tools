/* Simple plugin example  I/O port is a Forth command console. */

#include "base.h"
#include "plugin_api.h"


/* TAG_PLUGIO stream will be routed here. */
static void plugin_put(uint8_t byte) {
}
static void plugin_write(const uint8_t *buf, uint32_t len) {
    while(len--) { plugin_put(*buf++); }
}
static uint32_t plugin_read(uint8_t *buf, uint32_t size) {
    return 0;
}

/* Loader calls this after reflashing. */

static void plugin_start(void) {
    plugin_init_memory();
    infof("plugin_start OK\n");
}

/* Header at start of .bin file */
struct plugin_service plugin PLUGIN_HEADER_SECTION = {
    .version = PLUGIN_API_VERSION,
    .io = { .read = plugin_read, .write = plugin_write },
    .start = plugin_start
};



#if 0
// Defined in doodle.c
void set_pin(int pin, int val);
KEEP void set_pin_delegate(int pin, int val) {
    set_pin(pin, val);
}
#endif



