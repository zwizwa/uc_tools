// Wave form generator.
// See etf_test.c for a simpler ETF protocol example.

#include "sm_etf.h"
#include "base.h"
#include "gdbstub_api.h"
#include <string.h>

uint32_t dispatch(uint8_t type,
                  uint8_t *buf, uint32_t buf_len,
                  int32_t *tag, uint32_t tag_len) {
    infof("wavegen:(%d,%d)", type, buf_len);
    for(int i=0; i<=tag_len; i++) { infof(",%d", tag[i]); }
    infof("\n");
    return 0;
}


/* ETF IO */
struct sm_etf sm_etf;
uint8_t etf_buf[1024];

static uint32_t cb(struct sm_etf *sm) {
    return dispatch(sm->data_type, sm->buf, sm->data_size, sm->stack, sm->depth);
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
    return etf_tagged_read(123, info_read, buf, len);
}
const struct gdbstub_io etf_io = {
    .read  = etf_read,
    .write = etf_write,
};

/* STARTUP */
void switch_protocol(const uint8_t *buf, uint32_t size) {
    *_service.io = (struct gdbstub_io *)(&etf_io);
    (*_service.io)->write(buf, size);
}
void start(void) {
    hw_app_init();
    rcc_periph_clock_enable(RCC_GPIOA | RCC_AFIO);
    sm_etf_reset();
}
void stop(void) {
    hw_app_stop();
    _service.reset();
}
int main(void) { start(); }

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "Waveform Generator";
const char config_serial[]       CONFIG_DATA_SECTION = "000000000000000000000000";
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




