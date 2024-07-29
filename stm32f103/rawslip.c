/* Raw slip example. */

#include "base.h"
#include "gdbstub_api.h"
#include <string.h>

#include "sliplib.h"

#include "pbuf.h"

uint8_t pbuf_in_buf[1024];
struct pbuf pbuf_in = PBUF_INIT_FROM_BUF(pbuf_in_buf);

void app_write_byte(struct slip_write_state *s, uint8_t byte) {
    pbuf_put(&pbuf_in, byte);
}
void app_write_end(struct slip_write_state *s) {
    struct pbuf *p = &pbuf_in;
    if (p->count >= 2) {
        uint16_t tag = read_be(&p->buf[0], 2);
        switch(tag) {
        case TAG_GDB:
            // infof("write TAG_GDB %d\n", p->count-2);
            _service.rsp_io.write(&p->buf[2], p->count-2);
            break;
        default:
            break;
        }
    }
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
    if ((rv = slip_read_tagged(TAG_INFO, info_read, buf, room))) return rv;
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

extern uint32_t _eflash;

static void switch_protocol(const uint8_t *buf, uint32_t size) {
    infof("rawslip.c: SLIP on serial port.\n");
    infof("_eflash = 0x%08x (0x%08x)\n", &_eflash, _eflash);
    *_service.io = (struct gdbstub_io *)(&app_io);
    (*_service.io)->write(buf, size);
}


void start(void) {
    /* Low level application init.  Note that this needs to be called
     * manually after loading to initialize memory. */
    hw_app_init();
}

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "rawslip";
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;
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



