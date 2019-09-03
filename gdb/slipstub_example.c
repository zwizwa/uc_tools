/* Example boilerplate for a slipstub application.
   This has the following handled behind the scene:
   - slip input -> flat packet conversion
   - handle GDB stub, PING incoming packets
   - handle GDB stub, LOG outgoing packets
*/

#include "base.h"
#include "gdbstub_api.h"
#include <string.h>

#include "slipstub.h"

/* Buffers need to be reserved in the main app. */
struct cbuf cbuf_from_usb; uint8_t cbuf_from_usb_buf[4];
struct cbuf cbuf_to_usb;   uint8_t cbuf_to_usb_buf[1024];
struct pbuf pbuf_from_usb; uint8_t pbuf_from_usb_buf[1024];


KEEP void set_pin(int pin, int val) {
    hw_gpio_write(GPIOA,pin,val);
    hw_gpio_config(GPIOA,pin,HW_GPIO_CONFIG_OUTPUT);
}


/* This function receives complete SLIP packets from USB.
   Note that the pbuf contains the tag in the first 2 bytes. */
static void dispatch(struct slipstub *s, uint16_t tag, const struct pbuf *p) {
    switch(tag) {
    case 1:
        if (p->count < 4) return;
        set_pin(p->buf[2], p->buf[3]);
        cbuf_write_slip_reply(&cbuf_to_usb, p, 4, 0, 0);
    default:
        infof("bad tag %04x", tag);
    }
}

/* Poll I/O and write a message on change. */
uint8_t last_io_state;
static void poll(void) {
    uint8_t io_state =
        hw_gpio_read(GPIOA,0) |
        (hw_gpio_read(GPIOA,1) << 1);
    if (io_state != last_io_state) {
        cbuf_write_slip_tagged(&cbuf_to_usb, 1, &io_state, 1);
        last_io_state = io_state;
    }
}


/* GDBSTUB / SLIP BOILERPLATE */

struct slipstub slipstub = {
    .slip_in   = &cbuf_from_usb,
    .packet_in = &pbuf_from_usb,
    .slip_out  = &cbuf_to_usb,
    .dispatch  = dispatch
};

void start(void) {
    hw_app_init();
    rcc_periph_clock_enable(RCC_GPIOA | RCC_AFIO);
    CBUF_INIT(cbuf_from_usb);
    CBUF_INIT(cbuf_to_usb);
    PBUF_INIT(pbuf_from_usb);
    _service.add(poll);
}
void stop(void) {
    hw_app_stop();
    _service.reset();
}

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "SLIP stub test";
const char config_serial[]       CONFIG_DATA_SECTION = "2";
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;
const char config_version[]      CONFIG_DATA_SECTION = BUILD;
const char config_protocol[]     CONFIG_DATA_SECTION = "slip";

struct gdbstub_config config CONFIG_HEADER_SECTION = {
    .manufacturer    = config_manufacturer,
    .product         = config_product,
    .serial          = config_serial,
    .firmware        = config_firmware,
    .version         = config_version,
    .protocol        = config_protocol,
    .start           = start,
    .stop            = stop,
    .switch_protocol = slipstub_switch_protocol,
};



