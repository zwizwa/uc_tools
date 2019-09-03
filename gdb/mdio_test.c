/* MDIO Ethernet PHY test. */

#include "base.h"
#include "gdbstub_api.h"
#include <string.h>

#include "slipstub.h"

#include "cbuf.h"
#include "pbuf.h"
struct cbuf cbuf_from_usb; uint8_t cbuf_from_usb_buf[4];
struct cbuf cbuf_to_usb;   uint8_t cbuf_to_usb_buf[1024];
struct pbuf pbuf_from_usb; uint8_t pbuf_from_usb_buf[1024];

#include "mdio.h"

#define MDIO_DATA  GPIOA,0
#define MDIO_CLOCK GPIOA,1

int mdio_get_data() {
    return hw_gpio_read(MDIO_DATA);
}
void mdio_set_data(int v) {
    hw_gpio_write(MDIO_DATA,v);
}
void mdio_set_clock(int v) {
    hw_gpio_write(MDIO_CLOCK,v);
}
void mdio_set_dir(int d) {
    uint32_t config = d ? HW_GPIO_CONFIG_OUTPUT : HW_GPIO_CONFIG_INPUT_PULL;
    hw_gpio_write(MDIO_DATA,1);
    hw_gpio_config(MDIO_DATA,config);
}
void mdio_delay(void) {
    // hw_busywait(HW_LOOPS_PER_US/2);
    hw_busywait_ms(1);
}





KEEP void set_pin(int pin, int val) {
    hw_gpio_write(GPIOA,pin,val);
    hw_gpio_config(GPIOA,pin,HW_GPIO_CONFIG_OUTPUT);
}
uint32_t nb_commands = 0;

/* SLIP data incoming from USB controller.
   Called by USB driver.
   Packets end ip in dispatch() */


static void dispatch(struct slipstub *s, uint16_t tag, const struct pbuf *p) {
    switch(tag) {
    case 1:
        if (p->count < 4) return;
        set_pin(p->buf[2], p->buf[3]);
        cbuf_write_slip_reply(&cbuf_to_usb, p, 4, 0, 0);
    case 2: {
        if (p->count < 4) return;
        uint8_t  phy = *(int*)(p->buf+2);
        uint8_t  reg = *(int*)(p->buf+3);
        uint16_t val = mdio_read(phy, reg);
        infof("mdio_read %d %d -> %d\n", phy, reg, val);
        cbuf_write_slip_reply(&cbuf_to_usb, p, 4, (uint8_t*)&val, sizeof(val));
        break;
    }
    case 3: {
        if (p->count < 6) return;
        uint8_t  phy = *(int*)(p->buf+2);
        uint8_t  reg = *(int*)(p->buf+3);
        uint16_t val = *(int*)(p->buf+4);
        infof("mdio_write %d %d %d\n", phy, reg, val);
        mdio_write(phy, reg, val);
        cbuf_write_slip_reply(&cbuf_to_usb, p, 6, 0, 0);
        break;
    }
    default:
        infof("bad tag %04x", tag);
    }
}

uint8_t last_io_state;
static void poll(void) {
}
void poll_io(void) {
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
    hw_gpio_config(MDIO_CLOCK,HW_GPIO_CONFIG_OUTPUT);
    _service.add(poll);

    uint16_t r2 = mdio_read(1, 2);
    uint16_t r3 = mdio_read(1, 3);
    infof("phy: ID %04x:%04x\n", r2, r3);

}
void stop(void) {
    hw_app_stop();
    _service.reset();
}

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "MDIO Test Board";
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



