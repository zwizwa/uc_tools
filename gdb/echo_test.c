/* A simple console echo application, demonstrating application
 * startup and the default log buffer. */

#include "base.h"
#include "gdbstub_api.h"
#include <string.h>

/* Called on USB OUT token. */
static void console_write(const uint8_t *buf, uint32_t len) {
    for(uint32_t i = 0; i < len; i++) {
        if(buf[i] == '\r') info_putchar('\n');
        info_putchar(buf[i]);
    }
}
/* Called on USB IN token.  Returns 0 when there is no data. */
static uint32_t console_read(uint8_t *buf, uint32_t len) {
    return info_read(buf, len);
}
const struct gdbstub_io console_io = {
    .read  = console_read,
    .write = console_write,
};



// BOILERPLATE


/* If bootloader sees a message that does not parse as GDB RSP, it
   passes it here so we can install a new i/o handler on the virtual
   serial port. */
void switch_to_console_io(const uint8_t *buf, uint32_t size) {
    infof("Switched protocol to console_io\n");
    *_service.io = (struct gdbstub_io *)(&console_io);
    (*_service.io)->write(buf, size);
}

void start(void) {
    /* Low level application init.  Note that this performs memory
     * initialization that would normally happen before main() is
     * called. */
    hw_app_init();

    /* IO init */
    rcc_periph_clock_enable(RCC_GPIOA | RCC_AFIO);
}
void stop(void) {
    hw_app_stop();
    _service.reset();
}

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "Echo Test";
const char config_serial[]       CONFIG_DATA_SECTION = "0";
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;

struct gdbstub_config config CONFIG_HEADER_SECTION = {
    .manufacturer    = config_manufacturer,
    .product         = config_product,
    .serial          = config_serial,
    .firmware        = config_firmware,
    .version         = config_version,
    .start           = start,
    .stop            = stop,
    .switch_protocol = switch_to_console_io,
};



