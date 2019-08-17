
// FIXME: Binary copy needs to be checked for oveflow, but there is
// still something wrong resulting in:
// f(B),B=binary:copy(<<0>>,100).
// gdbstub:dev({zoe,[9,2,4]}) ! {send, term_to_binary([{3,B},{1,B}])}.
//  {data,<<"¿¿:(109,100),3\ncb:(109,100),1\n">>}


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

// S=fun(T) -> gdbstub:dev({zoe,[9,2,3]}) ! {send, term_to_binary(T)} end.

// application will typically provide a top level callback
static uint32_t cb1(uint8_t type,
                uint8_t *buf, uint32_t buf_len,
                int32_t *tag, uint32_t tag_len) {
    infof("cb1:(%d,%d)", type, buf_len);
    for(int i=0; i<tag_len; i++) { infof(",%d", tag[i]); }
    infof("\n");
    return 0;
}
// but for sm_etf, the cb is kept simple: just pass the struct
static uint32_t cb(struct sm_etf *sm) {
    return cb1(sm->data_type, sm->buf, sm->data_size, sm->stack, sm->depth);
}

static void sm_etf_reset(void) {
    sm_etf_init(&sm_etf, &etf_buf[0], sizeof(etf_buf), &cb);
}

static void etf_write(const uint8_t *buf, uint32_t len) {
    uint32_t status = sm_etf_write(&sm_etf, buf, len);
    if (SM_WAITING != status) {
        infof("status=%x\n",status);
        sm_etf_reset();
    }
}



static uint32_t etf_read(uint8_t *buf, uint32_t len) {
    // Wrap console data in an ETF binary.  Note that pure binaries
    // would be easy to parse on the other end, but let's keep it
    // consistent.
    return etf_tagged_read(123, info_read, buf, len);
}


const struct gdbstub_io etf_io = {
    .read  = etf_read,
    .write = etf_write,
};



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



