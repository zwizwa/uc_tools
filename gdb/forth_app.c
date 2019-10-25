// Stand-alone wrapper for forth.c
// FIXME: Needs echo, \r\n and maybe small line editor?

#include "forth.c"
#include "gdbstub_api.h"

const struct gdbstub_io app_io = {
    .read  = forth_read,
    .write = forth_write,
};
static void switch_protocol(const uint8_t *buf, uint32_t size) {
    *_service.io = (struct gdbstub_io *)(&app_io);
    (*_service.io)->write(buf, size);
}
void start(void) {
    hw_app_init();
    forth_start();
}

// Note that manufacturer "Zwizwa" is special in that it plugs into
// exo.  Pick "Kmook" for stand-alone applications.  Zoo has a
// dedicated port to flash boards with stand-alone firmware.

const char config_manufacturer[] CONFIG_DATA_SECTION = "Kmook";
const char config_product[]      CONFIG_DATA_SECTION = "Forth";
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;

struct gdbstub_config config CONFIG_HEADER_SECTION = {
    .manufacturer    = config_manufacturer,
    .product         = config_product,
    .firmware        = config_firmware,
    .start           = start,
    .switch_protocol = switch_protocol,
};



