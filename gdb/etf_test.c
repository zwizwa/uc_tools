/* FIXME: test/sm_etf works as erlang port.  Next is to make it run
 * here on the uc with protocol switching, and add support for it in
 * gdbstub.. */



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
uint8_t etf_buf[1024];

static void sm_etf_reset(void) {
    sm_etf_init(&sm_etf, &etf_buf[0], sizeof(etf_buf));
}

static void etf_write(const uint8_t *buf, uint32_t len) {
    sm_etf_reset();
    sm_etf_write(&sm_etf, buf, len);
}
static uint32_t etf_read(uint8_t *buf, uint32_t len) {
    return info_read(buf, len);
}
const struct gdbstub_io etf_io = {
    .read  = etf_read,
    .write = etf_write,
};

void test(const uint8_t *buf, uint32_t len) {
    //for (int i=0; i<len; i++) { infof("%02x ", buf[i]); }
    //infof("\n");
    uint32_t status = sm_etf_write(&sm_etf, buf, len);
    infof("status: %08x\n", status);
}
#define TEST(...) { uint8_t buf[] = {__VA_ARGS__}; test(buf, sizeof(buf)); }

KEEP void test_1(void) {
    sm_etf_reset();
    TEST(131,
         LIST_EXT, 0, 0, 0, 1,
         SMALL_TUPLE_EXT, 2,
         SMALL_INTEGER_EXT, 101, // key

         LIST_EXT, 0, 0, 0, 1,   // value = dict
         SMALL_TUPLE_EXT, 2,
         SMALL_INTEGER_EXT, 102, // key
         SMALL_INTEGER_EXT, 103, // value
         NIL_EXT,

         NIL_EXT);
}


// BOILERPLATE


/* If bootloader sees a message that does not parse as GDB RSP, it
   passes it here so we can install a new i/o handler on the virtual
   serial port. */
void switch_protocol(const uint8_t *buf, uint32_t size) {
    *_service.io = (struct gdbstub_io *)(&etf_io);
    (*_service.io)->write(buf, size);
}

void start(void) {
    /* Low level application init.  Note that this performs memory
     * initialization that would normally happen before main() is
     * called. */
    hw_app_init();

    /* IO init */
    rcc_periph_clock_enable(RCC_GPIOA | RCC_AFIO);

    sm_etf_reset();

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



