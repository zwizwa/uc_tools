/* DHT11 interface

   Note that this is much easier to implement using
   blocking/busywaiting.

   However, this was an opportunity to explore event driven
   programming in a cleaner way, where events are abstracted and
   isolated hardware-dependent code implements the event propagation
   and setup.  This makes event-driven code testable.

   The hardware used in this example is:
   - A periodic interrupt to provide time-based events
   - Free-running timer readout
   - EXTI GPIO edge detection
   - GPIO read/write

   The there is an additional control for a high side PNP switch to
   feed the device 5V on the data line.  We turn that off when the
   device sends a message. A 100uF cap + diode is enough to bridge the
   communication time (20ms), causing about a 400mV drop in VCC down
   from about 4.2V.  Ask Tom for schematics.

*/

#include "base.h"
#include "infof.h"
#include "slipstub.h"

/* Buffers for SLIP I/O.  Input uses libstub.h to implement basic
 * behavior.  Output is a raw slip buffer. */
struct sbuf sbuf_from_usb; uint8_t sbuf_from_usb_buf[1024];
struct cbuf cbuf_to_usb;   uint8_t cbuf_to_usb_buf[1024];


#define DHT11_SLIP_CBUF (&cbuf_to_usb)
#define DHT11_POWER GPIOB,14
#define DHT11_COMM  GPIOB,15
#include "mod_dht11.c"



/* This function receives complete SLIP packets from USB.
   Note that the pbuf contains the tag in the first 2 bytes. */
static void dispatch(struct slipstub *s, uint16_t tag, const struct pbuf *p) {
    // infof("handle tag %04x\n", tag);
    switch(tag) {
    case 0x101:
        // FIXME: Send these out periodically maybe?
        // dht11 ! {send_packet,<<16#101:16>>}.
        // info ends up on usb, tagged with 0x101.
        // TAG_REPLY isn't really necessary. This will only have a
        // single process attached.
        // infof("dht_request()\n");
        dht11_request(&dht11);
        break;

#if 0 // example TAG_REPLY
    case TAG_PING:
        //infof("ping:%d\n",p->count-2);
        cbuf_write_slip_tagged(&slip_out, TAG_REPLY,
                               &p->buf[2], p->count-2);
        break;
#endif


    default:
        infof("bad tag\n");
    }
}

struct slipstub slipstub = {
    .slip_in   = &sbuf_from_usb.c,
    .packet_in = &sbuf_from_usb.p,
    .slip_out  = &cbuf_to_usb,
    .dispatch  = dispatch
};

#include "gdbstub_api.h"

const char config_product[];
void start(void) {
    /* Low level application init.  Note that this needs to be called
     * manually after loading to initialize memory. */
    hw_app_init();

    SBUF_INIT(sbuf_from_usb);
    CBUF_INIT(cbuf_to_usb);

    /* GPIO & EXTI */
    rcc_periph_clock_enable(RCC_GPIOA | RCC_AFIO);

    /* Use a single periodic timer to provide time base.  If the
     * application allows for it -- basically a power consumption
     * requirement because CPU will do more work -- this is almost
     * always simpler than messing with timer configurations
     * directly. */
    dht11_init();

    infof("product: %s\n",&config_product[0]);
}

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "DHT11 interface board";
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;
const char config_version[]      CONFIG_DATA_SECTION = BUILD;
const char config_protocol[]     CONFIG_DATA_SECTION = "{driver,dht11_slip,slip}";

struct gdbstub_config config CONFIG_HEADER_SECTION = {
    .manufacturer    = config_manufacturer,
    .product         = config_product,
    .firmware        = config_firmware,
    .version         = config_version,
    .protocol        = config_protocol,
    .start           = start,
    .switch_protocol = slipstub_switch_protocol,
};
