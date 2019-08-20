/* Ethernet testing.  Note that this does not make a whole lot of
   sense on its own over USB serial.  It's usually much easier to keep
   the protocol simple and have some driver on the host end.

   However, this code is intended to explore Ethernet development for
   two projects in progress:

   1) A standard RNDIS or CDC-ECM gadget.

   2) An Ethernet data pack/unpack application with
      - high bw MAC<->FPGA<->busses
      - low bw management application on uC

   For both we have Ethernet/UDP + possible minimalistic TCP on the
   uC.  The FPGA presents data frames over SPI.



*/

#include "base.h"
#include "gdbstub_api.h"
#include <string.h>
#include "cbuf.h"
#include "pbuf.h"

struct pbuf incoming; uint8_t incoming_buf[1518];
struct cbuf outgoing; uint8_t outgoing_buf[4096];

// Main rate is just base clock for audio
#define DIV 1500 // (/ 72000000 48000)
#define TIM_PERIODIC 4
static volatile uint32_t sample;
static volatile uint32_t sec;

static const struct hw_periodic hw_periodic_config[] = {
//          rcc       irq            tim   div  pre
//---------------------------------------------------
    [2] = { RCC_TIM2, NVIC_TIM2_IRQ, TIM2, DIV, 1 },
    [3] = { RCC_TIM3, NVIC_TIM3_IRQ, TIM3, DIV, 1 },
    [4] = { RCC_TIM4, NVIC_TIM4_IRQ, TIM4, DIV, 1 },
    [5] = { RCC_TIM5, NVIC_TIM5_IRQ, TIM5, DIV, 1 },
};
#define C_PERIODIC hw_periodic_config[TIM_PERIODIC]

void HW_TIM_ISR(TIM_PERIODIC)(void) {
    hw_periodic_ack(C_PERIODIC);
    sample++;
    if (sample == 48000) {
        sample = 0;
        sec++;
    }
}

static const uint8_t heartbeat[] = {
    // broadcast dest MAC
    0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
    // source MAC
    0xAE,0,0,0,0,1,
    // ethertype
    0x12,0x34,
    // payload
    0x56,0x78
};

void poll(void) {
    static uint32_t last_sec = 0;
    if (sec > last_sec) {
        last_sec++;
        uint8_t buf[4];
        write_be(buf, sizeof(heartbeat), 4);
        cbuf_write(&outgoing, buf, 4);
        cbuf_write(&outgoing, heartbeat, sizeof(heartbeat));
    }
}




void recv(void *ctx, struct pbuf *p) {
    if (p->count < 14) return; // incomplete ethernet header
}

/* Use zwizwa/udpbridge to connect this to a tap interface */
static void packet4_write(const uint8_t *buf, uint32_t len) {
    pbuf_packetn_write(&incoming, 4,
                       buf, len,
                       (pbuf_sink_t)recv, NULL);
}
static uint32_t packet4_read(uint8_t *buf, uint32_t len) {
    return cbuf_read(&outgoing, buf, len);
}
const struct gdbstub_io etf_io = {
    .read  = packet4_read,
    .write = packet4_write,
};
void switch_protocol(const uint8_t *buf, uint32_t size) {
    *_service.io = (struct gdbstub_io *)(&etf_io);
    (*_service.io)->write(buf, size);
}

void start(void) {
    /* Low level application init.  Note that this needs to be called
     * manually after loading to initialize memory. */
    hw_app_init();
    PBUF_INIT(incoming);
    CBUF_INIT(outgoing);
    hw_periodic_init(C_PERIODIC);
    _service.add(poll);

}

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "Ethernet Test Board";
const char config_serial[]       CONFIG_DATA_SECTION = "AE0000000001";
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;
const char config_version[]      CONFIG_DATA_SECTION = BUILD;
const char config_protocol[]     CONFIG_DATA_SECTION = "{ethernet,4}";

struct gdbstub_config config CONFIG_HEADER_SECTION = {
    .manufacturer    = config_manufacturer,
    .product         = config_product,
    .serial          = config_serial,
    .firmware        = config_firmware,
    .version         = config_version,
    .protocol        = config_protocol,
    .start           = start,
    .switch_protocol = switch_protocol,
};






