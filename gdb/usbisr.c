// Test for running USB from interrupt.
// Note that this needs a recent bootloader to work..

#include "base.h"
#include "gdbstub_api.h"
#include <string.h>




/* STARTUP */
void switch_protocol(const uint8_t *buf, uint32_t size) {
    // Don't
}
void start(void) {
    hw_app_init();
    rcc_periph_clock_enable(RCC_GPIOA | RCC_AFIO);
}
void stop(void) {
    hw_app_stop();
    _service.reset();
}
int main(void) { start(); }

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "USB ISR test";
const char config_serial[]       CONFIG_DATA_SECTION = "000000000000000000000000";
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;
const char config_version[]      CONFIG_DATA_SECTION = BUILD;

struct gdbstub_config config CONFIG_HEADER_SECTION = {
    .manufacturer    = config_manufacturer,
    .product         = config_product,
    .serial          = config_serial,
    .firmware        = config_firmware,
    .version         = config_version,
    .start           = start,
    .stop            = stop,
    .switch_protocol = switch_protocol,
    .flags           = GDBSTUB_CONFIG_USB_ISR
};




