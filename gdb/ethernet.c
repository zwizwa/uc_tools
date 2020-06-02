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

#include "ethernet."

struct pbuf incoming; uint8_t incoming_buf[1522];
struct cbuf outgoing; uint8_t outgoing_buf[4096];

// Main rate is just base clock for audio
#define DIV 1500 // (/ 72000000 48000)
#define TIM_PERIODIC 4
static volatile uint32_t sample;
static volatile uint32_t tick;
uint32_t tick_period = 4800;

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
    if (sample == tick_period) {
        sample = 0;
        tick++;
    }
}


uint32_t byte_count = 0;
uint32_t packet_count = 0;


// Multicast/broadcast seems most convenient.
// https://networklessons.com/multicast/multicast-ip-address-to-mac-address-mapping
// socat - UDP4-RECVFROM:1234,ip-add-membership=224.0.13.1:10.1.3.2,fork | hd
// socat - UDP4-RECVFROM:1234,fork | hd

#define SIP 10,1,3,123  // does this really matter? (only for SSM?)
#define SMAC 0xAE, 0, SIP
#if 1
// multicast
#define MULTICAST 0,13,1
#define DIP 224,MULTICAST
#define DMAC 0x01,0x00,0x5e,MULTICAST
#else
// broadcast
#define DIP 255,255,255,255
#define DMAC 255,255,255,255,255,255
#endif

struct __attribute__((packed)) headers {
    uint32_t len;
    struct mac m;
    struct ip  i;
    struct udp u;
};

static struct headers headers = {
    .m = {
        .d_mac = {DMAC},
        .s_mac = {SMAC},
        .ethertype = HTONS(0x0800), // IPv4
    },
    .i = {
        .version_ihl = 0x45,
        .dscp_ecn = 0,
        .identification = HTONS(0x1234), // ??
        .flags_fo = HTONS(0x4000), // don't fragment
        .ttl = 0x40,
        .protocol = 0x11, // UDP,
        .s_ip = {SIP},
        .d_ip = {DIP}
    },
    .u = {
        .s_port = HTONS(4321),
        .d_port = HTONS(1234),
    }
};

static void send_udp(const struct headers *headers,
                     const uint8_t *buf, uint32_t len) {
    struct headers h = *headers;
    h.i.total_length = htons(28 + len);
    h.i.header_checksum = ip_checksum(&h.i, sizeof(h.i));

    /* UDP checksum is optional for IPv4 so it is not set here.
     * Note that computation would be based on a pseudo IP header,
     * not the actual header. */
    h.u.length   = htons( 8 + len);
    h.u.checksum = 0;

    h.len = htonl(sizeof(h) - 4 + len);
    cbuf_write(&outgoing, (const uint8_t*)&h, sizeof(h));
    cbuf_write(&outgoing, buf, len);
}

uint32_t last_tick = 0;

static void send_status(void) {
    // Status info
    uint32_t buf[4] = {
        htonl(last_tick),
        htonl(byte_count),
        htonl(packet_count),
        0
    };
    send_udp(&headers, (const uint8_t*)&buf[0], sizeof(buf));
}

/* Makre sure we don't overflow the outgoing buffer. */
static uint32_t outgoing_room(uint32_t need) {
    uint32_t have = cbuf_room(&outgoing);
    if (have < sizeof(struct headers) + need) return 0;
    return have - sizeof(struct headers);
}


static void send_log(void) {
    if (!info_bytes()) return;

    /* Backpressure.  Don't send anything if there is not a minimal
     * payload size available. */
    uint32_t need = 100;
    uint32_t room = outgoing_room(need);
    if (!room) return;

    /* FIXME: avoid the buffer here.  Push header into cbuf and then
     * push payload here. */
    uint8_t buf[room];
    uint32_t len = info_read(buf, sizeof(buf));
    send_udp(&headers, (const uint8_t*)&buf[0], len);
}


void poll(void) {
    /* Note that there can be no direct path from recv() to a log
     * flush message going out, as we will receive our own broadcasts
     * and this would create a feedback loop.  Limit the rate by
     * polling. */
    if (tick != last_tick) {
        last_tick++;
        switch(2) {
        case 1: send_status(); break;
        case 2: send_log(); break;
        default: break;
        }
    }
}


void recv(void *ctx, const struct pbuf *p) {
    if (p->count < 14) return; // incomplete ethernet header
    // FIXME: check destination
    // FIXME: validate checksum
    const struct headers *h = (void*)&p->buf[0];
    uint16_t type = ntohs(h->m.ethertype);
    switch(type) {
        // Ignore noise
    //case 0x8100: // ARP
    //    break;
    default:
        infof("(%d) %04x %d\n", packet_count, type, p->count);
        break;
    }

    packet_count++;
}

/* Use zwizwa/udpbridge to connect this to a tap interface */
static void packet4_write(const uint8_t *buf, uint32_t len) {
    byte_count += len;
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

    infof("ethernet.c\n");
}

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "Ethernet Test Board";
const char config_serial[]       CONFIG_DATA_SECTION = "ae:00:00:00:00:01";
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;
const char config_version[]      CONFIG_DATA_SECTION = BUILD;
const char config_protocol[]     CONFIG_DATA_SECTION = "{driver,ethernet,{packet,4}}";

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






