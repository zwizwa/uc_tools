/* MDIO Ethernet PHY test. */

#include "base.h"
#include "gdbstub_api.h"
#include <string.h>


KEEP void set_pin(int pin, int val) {
    hw_gpio_write(GPIOA,pin,val);
    hw_gpio_config(GPIOA,pin,HW_GPIO_CONFIG_OUTPUT);
}
uint32_t nb_commands = 0;
static void command_write(const uint8_t *buf, uint32_t len) {
}
static uint32_t command_read(uint8_t *buf, uint32_t len) {
    return info_read(buf, len);
}
const struct gdbstub_io command_io = {
    .read  = command_read,
    .write = command_write,
};

void switch_protocol(const uint8_t *buf, uint32_t size) {
    infof("Console on serial port.\n");
    *_service.io = (struct gdbstub_io *)(&command_io);
    (*_service.io)->write(buf, size);
}

void start(void) {
    hw_app_init();
    rcc_periph_clock_enable(RCC_GPIOA | RCC_AFIO);
}
void stop(void) {
    hw_app_stop();
    _service.reset();
}

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "MDIO Board";
const char config_serial[]       CONFIG_DATA_SECTION = "2";
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
};



