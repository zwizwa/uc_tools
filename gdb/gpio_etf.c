// ETF-controlled GPIO acces


#include "sm_etf.h"
#include "base.h"
#include "gdbstub_api.h"
#include <string.h>

struct sm_etf sm_etf;
uint8_t etf_buf[1024];

#define ERR_CB = 0x100


void set_pin(uint32_t gpiodev, int pin, int val) {
    hw_gpio_write(gpiodev,pin,val);
    hw_gpio_config(gpiodev,pin,HW_GPIO_CONFIG_OUTPUT);
}

// The info buffer is used to send reply ETF data
#define INFO(...) { uint8_t buf[] = {__VA_ARGS__}; info_write(buf, sizeof(buf)); }

static uint32_t cb1(uint8_t type,
                    uint8_t *buf, uint32_t buf_len,
                    int32_t *tag, uint32_t tag_len) {
    // FIXME: don't rely on integer sizes
    if (SMALL_INTEGER_EXT == type && 1 == tag_len) {
        int32_t gpio;
        switch(type){
        case SMALL_INTEGER_EXT: gpio = *buf; break;
        case INTEGER_EXT: gpio = *(int32_t*)buf; break;
        default: return 0x100;
        }
        int pin = gpio % 32;
        int port = gpio / 32;
        int val = *buf;
        if (port < 0) return 0x101;
        if (port > 2) return 0x102;
        if (val < 0)  return 0x103;
        if (val > 1)  return 0x104;
        switch(port) {
        case 0: set_pin(GPIOA, pin, val); break;
        case 1: set_pin(GPIOB, pin, val); break;
        case 2: set_pin(GPIOC, pin, val); break;
        }
        return 0;
    }
    if (NIL_EXT == type && 1 == tag_len) {
        INFO(0,0,0,6,
             131,
             ATOM_EXT,0,2,'o','k');
        return 0;
    }
    return 0x105;
}
static uint32_t cb(struct sm_etf *sm) {
    return cb1(sm->data_type, sm->buf, sm->data_size, sm->stack, sm->depth);
}

static void sm_etf_reset(void) {
    sm_etf_init(&sm_etf, &etf_buf[0], sizeof(etf_buf), &cb);
}

static void etf_write(const uint8_t *buf, uint32_t len) {
    uint32_t status = sm_etf_write(&sm_etf, buf, len);
    if (SM_WAITING != status) {
        INFO(0,0,0,16,
             131,
             SMALL_TUPLE_EXT,2,
             ATOM_EXT,0,5,'e','r','r','o','r',
             INTEGER_EXT,
             status >> 24,
             status >> 16,
             status >> 8,
             status >> 0);
        sm_etf_reset();
    }
}

static uint32_t etf_read(uint8_t *buf, uint32_t len) {
    // Buffer contains ETF
    return info_read(buf, len);
}

const struct gdbstub_io etf_io = {
    .read  = etf_read,
    .write = etf_write,
};



// BOILERPLATE


/* If bootloader sees a message that does not parse as GDB RSP, it
   passes it here so we can install a new i/o handler on the virtual
   serial port. */
void switch_protocol(const uint8_t *buf, uint32_t size) {
    *_service.io = (struct gdbstub_io *)(&etf_io);
    (*_service.io)->write(buf, size);
}

void start(void) {
    /* Low level application init.  Note that this performs memory
     * initialization that would normally happen before main() is
     * called. */
    hw_app_init();

    /* IO init */
    rcc_periph_clock_enable(
        RCC_GPIOA |
        RCC_GPIOB |
        RCC_AFIO);
    sm_etf_reset();

}
void stop(void) {
    hw_app_stop();
    _service.reset();
}

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "ETF Test";
const char config_serial[]       CONFIG_DATA_SECTION = "0";
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;

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



