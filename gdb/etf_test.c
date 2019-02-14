/* Test application for ETF-based protocol. */

/* Subset of ETF protocol.  Basic principles:

   - A tree can be represented as [{Path,Leaf}]

   - This can easily be folded without needing intermediate storage,
     as long as the receiving end can parse the Leaf nodes in a
     streaming fashion.

   - The loop state that needs to be kept is just the path of the
     current leaf node.

   - By making all tags into uint32_t, the depth can be fixed to a
     particular size, avoiding memory allocation.

*/

#include "sm_etf.h"
#include "base.h"
#include "gdbstub_api.h"
#include <string.h>

struct sm_etf sm_etf;

const struct gdbstub_io **prev_proto;
int etf_busy = 0;
static void etf_write(const uint8_t *buf, uint32_t len) {
    if (len == 0) return;

    /* Fixme: let the sm give a return code. */
    if (!etf_busy && buf[0] != 131) {
        /* Allow protocol switches back to GDB RSP. */
        *_service.io = *prev_proto;
        (*_service.io)->write(buf, len);
        return;
    }
    /* Push the data into the state machine. */
    sm_etf_write(&sm_etf, buf, len);
}
static uint32_t etf_read(uint8_t *buf, uint32_t len) {
    return 0;
}
const struct gdbstub_io etf_io = {
    .read  = etf_read,
    .write = etf_write,
};

KEEP uint32_t test_1(void) {
    uint8_t buf[] = {131};
    return sm_etf_write(&sm_etf, buf, sizeof(buf));
}


// BOILERPLATE


/* If bootloader sees a message that does not parse as GDB RSP, it
   passes it here so we can install a new i/o handler on the virtual
   serial port. */
void switch_protocol(const uint8_t *buf, uint32_t size) {
    infof("Console on serial port.\n");
    *prev_proto = *_service.io;
    *_service.io = (struct gdbstub_io *)(&etf_io);
    (*_service.io)->write(buf, size);
}

void start(void) {
    /* Low level application init */
    hw_app_init();

    /* IO init */
    rcc_periph_clock_enable(RCC_GPIOA | RCC_AFIO);


}
void stop(void) {
    hw_app_stop();
    _service.reset();
}

int main(void) { start(); }

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "ETF Test";
const char config_serial[]       CONFIG_DATA_SECTION = "0";
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



