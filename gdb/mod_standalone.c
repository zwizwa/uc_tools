/* Stand alone board with command console. */

#include <stdint.h>
#include "base.h"
#include "gdbstub_api.h"
#include <string.h>

#include "instance.h"
#include "reset_device.h"
#include "cycle_counter.h"

#include "mod_console.c"

/* Provided by main file. */
void setup(void);
void loop(void);

#define MS_PERIODIC(var, ms) \
    CYCLE_COUNTER_PERIODIC(var, ((ms) * 72000))


/* STARTUP */
static void switch_protocol(const uint8_t *buf, uint32_t size) {
    *_service.io = (struct gdbstub_io *)(&standalone_io);
    (*_service.io)->write(buf, size);
}
extern const struct instance app;
void start(void) {
    hw_app_init();
    rcc_periph_clock_enable(RCC_GPIOA | RCC_GPIOB | RCC_AFIO);
    enable_cycle_counter();
    /* It's just so much better developing with a watchdog enabled... */
    iwdg_set_period_ms(1000);
    iwdg_start();
    _service.add(iwdg_reset);
    instance_need_top(32, &app);
}
void stop(void) {
    hw_app_stop();
    _service.reset();
}

#ifndef VERSION
#define VERSION "current"
#endif

#ifndef PRODUCT
#error need PRODUCT
#endif

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = PRODUCT;
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;
const char config_protocol[]     CONFIG_DATA_SECTION = "{driver,lab_board,slip}";

extern const char config_version[];

struct gdbstub_config config CONFIG_HEADER_SECTION = {
    .manufacturer    = config_manufacturer,
    .product         = config_product,
    .firmware        = config_firmware,
    .version         = config_version,
    .protocol        = config_protocol,
    .start           = start,
    .stop            = stop,
    .switch_protocol = switch_protocol,
    .monitor         = console_monitor,
};

