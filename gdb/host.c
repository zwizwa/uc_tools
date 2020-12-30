/* Minimal Flash / RAM plugin host.

   Use smaller slip decoder to keeps RAM usage minimal, leaving room
   for code in RAM. */

// FIXME: This doesn't work properly.  Use lab_board.c as a host for now.

#include "base.h"
#include "gdbstub_api.h"
#include <string.h>

#include "sliplib.h"

#include "pbuf.h"
#include "cbuf.h"

#include "plugin_api.h"

#include "crc.h"

#include "reset_device.h"


/* Run a timer interrupt in the background, to see if that keeps
   working.  This is pwm audio. */

/* This is a division scheme that has exact audio rate subsampling for
   8kHz audio, and integral second time keeping.  The PWM samples need
   to be scaled to fit the range.  That calcluation is combined with
   linear interpolation. */
#define AUDIO_DIV         1125  // giving 64kHz PWM rate, also PWM scale
#define AUDIO_RATE_KHZ    (72000 / AUDIO_DIV)

const struct hw_clockgen hw_audio_playback[] = {
//          rcc_tim   rcc_gpio   tim   gpio   pin div         duty          phase  pol chan     itr  irq (optional)
//-------------------------------------------------------------------------------------------------------------------
    [3] = { RCC_TIM3, RCC_GPIOB, TIM3, GPIOB, 0,  AUDIO_DIV,  AUDIO_DIV/2,  0,     1,  TIM_OC3, -1,  NVIC_TIM3_IRQ },

};
#define TIM_AUDIO 3
#define C_AUDIO hw_audio_playback[TIM_AUDIO]

volatile uint32_t audio_count;

void HW_TIM_ISR(TIM_AUDIO)(void) {
    hw_clockgen_ack(C_AUDIO);
    audio_count++;
}





/* See plugin_handle_message.
   Required buffer size is Flash block size + some header space. */
uint8_t packet_in_buf[1024 + 64];
struct pbuf packet_in;

uint8_t slip_out_buf[16];
struct cbuf slip_out;

void app_write_byte(struct slip_write_state *s, uint8_t byte) {
    pbuf_put(&packet_in, byte);
}
void app_write_end(struct slip_write_state *s) {
    struct pbuf *p = &packet_in;

    if (plugin_handle_message(p->buf, p->count)) goto done;

    if (p->count >= 2) {
        uint16_t tag = read_be(&p->buf[0], 2);
        switch(tag) {
        case TAG_PING:
            //infof("ping:%d\n",p->count-2);
            cbuf_write_slip_tagged(&slip_out, TAG_REPLY,
                                   &p->buf[2], p->count-2);
            break;
        case TAG_RESET:
            reset_device();
            infof("reset failed\n");
            break;
        case TAG_GDB:
            // infof("write TAG_GDB %d\n", p->count-2);
            _service.rsp_io.write(&p->buf[2], p->count-2);
            break;
        case TAG_CHECKSUM:
            // obj:call(bb1, {call, <<16#FFF9:16,16#08000000:32,1000:32>>}, 1000).
            // FIXME: this returns <<0,0,0,0,0,0,0,0>>
            if (p->count >= 2 + 4 + 4) {
                uint32_t start = read_be(&p->buf[2], 4);
                uint32_t len   = read_be(&p->buf[6], 4);
                uint32_t checksum = crc32b((uint8_t*)start, len);
                infof("checksum = 0x%x (%d bytes at 0x%x)\n", checksum, len, start);
                uint32_t reply_tag_len = p->count -  2 + 4 + 4;
                uint8_t reply[4 + reply_tag_len];
                memcpy(reply, p->buf + 2 + 4 + 4, reply_tag_len);
                reply[reply_tag_len]   = checksum >> 24;
                reply[reply_tag_len+1] = checksum >> 16;
                reply[reply_tag_len+2] = checksum >> 8;
                reply[reply_tag_len+3] = checksum >> 0;
                cbuf_write_slip_tagged(&slip_out, TAG_REPLY, reply, reply_tag_len + 4);
            }
            break;
        default:
            break;
        }
    }
  done:
    p->count = 0;
}

struct slip_write_state slip_write_state = {
    .byte = app_write_byte,
    .end  = app_write_end,
};
static void app_write(const uint8_t *buf, uint32_t len) {
    slip_write_tagged(&slip_write_state, buf, len);
}
static uint32_t app_read(uint8_t *buf, uint32_t room) {
    int rv;
    /* To respect message boundaries, slip_out is always read until it
       is empty before moving on to anything else. */
    if ((rv = cbuf_read(&slip_out, buf, room))) return rv;
    /* Then only do one of these per buffer.  The main reason for that
       is to guarantee some minimal available space, but it might not
       be necessary. */
    if ((rv = slip_read_tagged(TAG_INFO,   info_read, buf, room))) return rv;
    if ((rv = slip_read_tagged(TAG_PLUGIO, plugin_read, buf, room))) return rv;
    if ((rv = slip_read_tagged(TAG_GDB, _service.rsp_io.read, buf, room))) {
        // infof("read TAG_GDB %d\n", rv);
        return rv;
    }
    return 0;
}

const struct gdbstub_io app_io = {
    .read  = app_read,
    .write = app_write,
};

void info_firmware(void);
static void switch_protocol(const uint8_t *buf, uint32_t size) {
    info_firmware();
    *_service.io = (struct gdbstub_io *)(&app_io);
    (*_service.io)->write(buf, size);
}


void start(void) {
    /* Low level application init.  Note that this needs to be called
     * manually after loading to initialize memory. */
    hw_app_init();
    CBUF_INIT(slip_out);
    PBUF_INIT(packet_in);
#if 1
    hw_clockgen_init(C_AUDIO);
    hw_clockgen_arm(C_AUDIO);
    hw_clockgen_trigger(C_AUDIO);
#endif

}

#ifndef PRODUCT
#define PRODUCT "host"
#endif

#ifndef VERSION
#define VERSION "current"
#endif

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = PRODUCT;
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;
const char config_version[]      CONFIG_DATA_SECTION = VERSION;

// FIXME: Currently bootstrapped in lab_board
//const char config_protocol[]     CONFIG_DATA_SECTION = "{driver,host_slip,slip}";
const char config_protocol[]     CONFIG_DATA_SECTION = "{driver,lab_board,slip}";

struct gdbstub_config config CONFIG_HEADER_SECTION = {
    .manufacturer    = config_manufacturer,
    .product         = config_product,
    .firmware        = config_firmware,
    .version         = config_version,
    .protocol        = config_protocol,
    .start           = start,
    .switch_protocol = switch_protocol,
};
void info_firmware(void) {
    infof("%s: SLIP on serial port.\n", config_product);
    infof("_flash_free = 0x%08x\n", &_flash_free);
    infof("_ebss       = 0x%08x\n", &_ebss);
}


