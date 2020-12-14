/* Generic lab module.
   See e.g. lab1.c */

#include <stdint.h>
#include "base.h"

#include "gdbstub_api.h"
#include <string.h>


/* Protocol-wise we don't really need anything special, so use
   slipstub_buffered to do the basics (SLIP framing, PING, GDB, INFO)
   and use tag_u32 for app-specific commands. */
#include "slipstub.h"
struct slipstub slipstub;
struct slipstub_buffers slipstub_buffers;

#include "tag_u32.h"

/* Provided by main file. */
int handle_tag_u32(
    void *context,
    const uint32_t *arg,  uint32_t nb_args,
    const uint8_t *bytes, uint32_t nb_bytes);
void setup(void);
void loop(void);

void handle_tag(struct slipstub *s, uint16_t tag, const struct pbuf *p) {
    //infof("tag %d\n", tag);
    switch(tag) {
    case TAG_U32: {
        /* name ! {send_u32, [101, 1000000000, 1,2,3]}. */
        int rv = tag_u32_dispatch(handle_tag_u32, NULL, p->buf, p->count);
        if (rv) { infof("tag_u32_dispatch returned %d\n", rv); }
        break;
    }
    default:
        infof("unknown tag 0x%x\n", tag);
    }
}

/* Keep the interface symmetric. */
void send_tag_u32(
    void *context, /* Why is this here? */
    const uint32_t *arg,  uint32_t nb_args,
    const uint8_t *bytes, uint32_t nb_bytes) {

    uint8_t hdr[] = {U16_BE(TAG_U32), U16_BE(nb_args)};
    struct cbuf *b = slipstub.slip_out;
    cbuf_put(b, SLIP_END);
    cbuf_append_slip(b, hdr, sizeof(hdr));
    for (uint32_t i=0; i<nb_args; i++) {
        uint8_t a[] = {U32_BE(arg[i])};
        cbuf_append_slip(b, a, sizeof(a));
    }
    cbuf_append_slip(b, bytes, nb_bytes);
    cbuf_put(b, SLIP_END);
}
#define SEND_TAG_U32(...) {                                     \
        uint32_t a[] = { __VA_ARGS__ };                         \
        send_tag_u32(NULL,a,sizeof(a)/sizeof(uint32_t),NULL,0); \
}

/* STARTUP */


void start(void) {
    hw_app_init();
    /* FIXME: This assumes it's GPIOA */
    rcc_periph_clock_enable(RCC_GPIOA | RCC_GPIOB | RCC_AFIO);

    /* Use framwork for handling incoming USB SLIP commands. */
    slipstub_init(handle_tag);

    setup();
    _service.add(loop);
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
const char config_version[]      CONFIG_DATA_SECTION = VERSION;
const char config_protocol[]     CONFIG_DATA_SECTION = "{driver,lab_board,slip}";

struct gdbstub_config config CONFIG_HEADER_SECTION = {
    .manufacturer    = config_manufacturer,
    .product         = config_product,
    .firmware        = config_firmware,
    .version         = config_version,
    .protocol        = config_protocol,
    .start           = start,
    .stop            = stop,
    .switch_protocol = slipstub_switch_protocol,
};

