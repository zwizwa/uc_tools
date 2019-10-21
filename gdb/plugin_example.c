/* Simple plugin example  I/O port is a Forth command console. */

#include "base.h"
#include "plugin_api.h"

static void plug_write(const uint8_t *buf, uint32_t len) {
}
static uint32_t plug_read(uint8_t *buf, uint32_t size) {
    return 0;
}

struct plugin_service plugin PLUGIN_HEADER_SECTION = {
    .version = PLUGIN_API_VERSION,
    .io = { .read = plug_read, .write = plug_write }
};



#if 0
// Defined in doodle.c
void set_pin(int pin, int val);
KEEP void set_pin_delegate(int pin, int val) {
    set_pin(pin, val);
}
#endif



