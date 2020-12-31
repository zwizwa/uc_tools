/* Simple A/V core test: oled display, pwm audio. */

#include "generic.h"
#include "base.h"
#include "gdbstub_api.h"
#include <string.h>
#include "playback.h"
#include "memory.h"
#include "gdbstub.h"
#include "pbuf.h"
#include "cbuf.h"
#include "sliplib.h"
#include "oled.h"

struct cbuf cbuf_from_usb; uint8_t cbuf_from_usb_buf[4];
struct cbuf cbuf_to_usb;   uint8_t cbuf_to_usb_buf[1024];

struct pbuf pbuf_to_dispatch; uint8_t pbuf_to_dispatch_buf[1024];


/* Use modules with defaults. */
#include "mod_oled.c"



void to_hex_u32(char *buf, uint32_t val) {
    const char hex[] = "0123456789abcdef";
    for (int i=0; i<8; i++) {
        buf[i] = hex[0xF & (val >> ((7-i)*4))];
    }
}
#include "xorshift.h"

/* TODO: Generate a display menu on the fly from a pointer into a data
   structure, instead of drawing it first to a frame buffer.  Why does
   this matter?  It will render only when needed.
*/


void animation_poll(void) {
    char buf[10] = {};
    if (video_sync()) {
        // infof("sync\n");
        if (1) {
            if (0) {
                int last = 0;
                int cur = 0;
                for (int i=0; i<sizeof(oled_fb); i+=2) {
                    cur = !!(*(int8_t *)(0x20000000 + i) & 5);
                    uint8_t v;
                    if (last != cur) v = 0b01111110;
                    else if (cur)    v = 0b00000010;
                    else             v = 0b01000000;
                    oled_fb[i] = v;
                    if (cur)         v = 0b00000010;
                    else             v = 0b01000000;
                    oled_fb[i+1] = v;
                    last = cur;
                }
            }
            if (1) {
                static int skip = 0;
                if (!skip--) {
                    skip = 0; // random_u32() & 0x7;
                    uint32_t *fb = (void*)oled_fb;
                    for (int i=0; i<sizeof(oled_fb)/sizeof(*fb); i++) {
                        fb[i] = random_u32();
                    }
                }
            }
            oled_send_fb();
            to_hex_u32(buf, video_pulse); oled_send_text(1,10,buf);
        }
        else {
            oled_send_text(0,0,"hello friend");
            to_hex_u32(buf, video_pulse); oled_send_text(1,10,buf);
            to_hex_u32(buf, audio_pulse); oled_send_text(2,10,buf);
        }
    }
}


void usb_rx_dispatch(void *ctx, const struct pbuf *p) {
    if (p->count < 2) {
        infof("ignoring p->count=%d\n", p->count);
        return;
    }
    uint16_t tag = read_be(p->buf, 2);
    switch(tag) {
    case TAG_PING:
        cbuf_write_slip_tagged(&cbuf_to_usb, TAG_REPLY, &p->buf[2], p->count-2);
        return;
    case TAG_GDB:
        // infof("tag_gdb: %d\n", p->count);
        _service.rsp_io.write(&p->buf[2], p->count-2);
        break;
    default:
        infof("bad tag %04x\n", tag);
    }
}

/* SLIP data incoming from USB controller.
   Called by USB driver.
   Packets end ip in dispatch() */
static void usb_rx(const uint8_t *buf, uint32_t len) {
    pbuf_slip_write(
        buf, len,
        &cbuf_from_usb,    // intermediate cbuf for slip data
        &pbuf_to_dispatch, // incoming tagged packet
        usb_rx_dispatch, NULL);
}


/* Drain bufer to USB controller.
   Called by USB subsystem when ready to transmit. */
static uint32_t usb_tx(uint8_t *buf, uint32_t room) {
    int rv;
    /* To respect message boundaries, slip_out is always read until it
       is empty before moving on to anything else. */
    if ((rv = cbuf_read(&cbuf_to_usb, buf, room))) return rv;
    /* Then only do one of these per buffer.  The main reason for that
       is to guarantee some minimal available space, but it might not
       be necessary. */
    if ((rv = slip_read_tagged(TAG_INFO,   info_read, buf, room))) return rv;
    //if ((rv = slip_read_tagged(TAG_PLUGIO, plugin_read, buf, room))) return rv;
    if ((rv = slip_read_tagged(TAG_GDB, _service.rsp_io.read, buf, room))) {
        // infof("read TAG_GDB %d\n", rv);
        return rv;
    }
    return 0;
}



/* ADC */
void adc_init(void) {
    
}




/* ******** GDBSTUB GLUE */



const struct gdbstub_io app_io = {
    .write = usb_rx,
    .read  = usb_tx,
};


/* Bootloader starts in GDB RSP (GDBSTUB) mode.  If it sees a message
   that it cannot parse, it will call this function.  We install a new
   I/O handler on the USB ttyACM port. */
void switch_protocol(const uint8_t *buf, uint32_t size) {
    infof("Switch to SLIP\n");
    *_service.io = (struct gdbstub_io *)(&app_io);
    (*_service.io)->write(buf, size);
}


/* Called by bootloader before calling any other application code,
 * e.g. before calling switch_protocol()
 */


const char config_product[];


void start(void) {



    /* Set app interrupt vector table and initialize app memory. */
    hw_app_init();

    rcc_periph_clock_enable(RCC_GPIOA);
    rcc_periph_clock_enable(RCC_GPIOB);


    /* Buffer init */
    PBUF_INIT(pbuf_to_dispatch);
    CBUF_INIT(cbuf_to_usb);
    CBUF_INIT(cbuf_from_usb);

    /* Add main loop polling tasks. */
    _service.add(animation_poll);

    /* OLED driver */
    oled_init();
    oled_clear();

    audio_init();

    adc_init();

    scan_samples((void*)0x8008000);
    play_sample(1);

    infof("BOOT1=%d\n", hw_gpio_read(GPIOB,2));
    infof("MS_PER_TICK=%d\n", MS_PER_TICK);
    infof("TIM_AUDIO: pri=%d\n", NVIC_IPR(C_AUDIO.irq));
    infof("\n");
    infof("product: %s\n",&config_product[0]);

}


/* For debug purposes.  Normally, code will run indefinitely. */

void stop(void) {
    hw_app_stop();
    _service.reset();
}

#ifndef VERSION
#define VERSION "current"
#endif


/* Application description for bootloader.  Stored in Flash at a fixed location. */
const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "OLED+PWM test";
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;
const char config_protocol[]     CONFIG_DATA_SECTION = "slip";

struct gdbstub_config config CONFIG_HEADER_SECTION = {
    .manufacturer    = config_manufacturer,
    .product         = config_product,
    .firmware        = config_firmware,
    .version         = config_version,
    .protocol        = config_protocol,
    .start           = start,
    .stop            = stop,
    .switch_protocol = switch_protocol,
};

