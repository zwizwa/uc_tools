
#include <stdint.h>
#include "fixedpoint.h"
#include "base.h"

#include "gdbstub_api.h"
#include <string.h>



/* Protocol-wise we don't really need anything special, so use
   slipstub_buffered to do the basics (SLIP framing, PING, GDB, INFO)
   and use tag_u32 for app-specific commands. */
#include "slipstub.h"
struct slipstub slipstub;
struct slipstub_buffers slipstub_buffers;


/* CONFIGURATION */

#include "mod_synth.c"


/* COMMUNICATION */


/* slipstub calls this one for application tags.  We then patch
   through to command handler. */

int handle_tag_u32(
    void *context,
    const uint32_t *arg,  uint32_t nb_args,
    const uint8_t *bytes, uint32_t nb_bytes) {

    return synth_handle_tag_u32(context, arg, nb_args, bytes, nb_bytes);
}


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

#include "crc.h"
extern uint8_t _firmware_start;
extern uint8_t _firmware_endx;
uint32_t firmware_crc(void) {
    uint32_t size = &_firmware_endx - &_firmware_start;
    uint32_t crc = crc32b(&_firmware_start, size);
    infof("_firmware_start: 0x%08x\n", &_firmware_start);
    infof("_firmware_endx:  0x%08x\n", &_firmware_endx);
    infof("size:            0x%08x\n", size);
    infof("crc:             0x%08x\n", crc);
    return crc;
}



/* STARTUP */


void start(void) {
    hw_app_init();
    /* FIXME: This assumes it's GPIOA */
    rcc_periph_clock_enable(RCC_GPIOA | RCC_GPIOB | RCC_AFIO);

    /* Use framwork for handling incoming USB SLIP commands. */
    slipstub_init(handle_tag);

    /* Turn off the LED.  It introduces too much noise. */
    hw_gpio_config(GPIOC,13,HW_GPIO_CONFIG_INPUT);

    /* Init the synth time stack. */
    synth_init();

    //infof("C_CONTROL.div = %d\n", C_CONTROL.div);
    firmware_crc();

}
void stop(void) {
    hw_app_stop();
    _service.reset();
}

#ifndef VERSION
#define VERSION "current"
#endif

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "PDM CV Synth";
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;
const char config_version[]      CONFIG_DATA_SECTION = VERSION;
const char config_protocol[]     CONFIG_DATA_SECTION = "{driver,pdm,slip}";

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

