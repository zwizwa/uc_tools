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

/* See plugin_write_message.
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

    if (plugin_write_message(p->buf, p->count)) goto done;

    if (p->count >= 2) {
        uint16_t tag = read_be(&p->buf[0], 2);
        switch(tag) {
        case TAG_PING:
            //infof("ping:%d\n",p->count-2);
            cbuf_write_slip_tagged(&slip_out, TAG_REPLY,
                                   &p->buf[2], p->count-2);
            break;
        case TAG_GDB:
            // infof("write TAG_GDB %d\n", p->count-2);
            _service.rsp_io.write(&p->buf[2], p->count-2);
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
    if ((rv = cbuf_read(&slip_out, buf, room))) return rv; // drain this first
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

static void switch_protocol(const uint8_t *buf, uint32_t size) {
    infof("host.c: SLIP on serial port.\n");
    infof("_eflash = 0x%08x\n", &_eflash);
    infof("_ebss   = 0x%08x\n", &_ebss);
    *_service.io = (struct gdbstub_io *)(&app_io);
    (*_service.io)->write(buf, size);
}


void start(void) {
    /* Low level application init.  Note that this needs to be called
     * manually after loading to initialize memory. */
    hw_app_init();
    CBUF_INIT(slip_out);
    PBUF_INIT(packet_in);
}

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "host";
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;
const char config_version[]      CONFIG_DATA_SECTION = BUILD;
const char config_protocol[]     CONFIG_DATA_SECTION = "slip";

struct gdbstub_config config CONFIG_HEADER_SECTION = {
    .manufacturer    = config_manufacturer,
    .product         = config_product,
    .firmware        = config_firmware,
    .version         = config_version,
    .protocol        = config_protocol,
    .start           = start,
    .switch_protocol = switch_protocol,
};



