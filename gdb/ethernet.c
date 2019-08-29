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


#define NTOHS(w) ((((w)&0xFF00) >> 8) | (((w)&0x00FF) << 8))
#define HTONS(w) NTOHS(w)

#define NTOHL(w) (NTOHS((w)>>16) | (NTOHS(w)<<16))
#define HTONL(w) NTOHL(w)


static inline uint16_t ntohs(uint16_t w) { return NTOHS(w); }
static inline uint16_t htons(uint16_t w) { return HTONS(w); }
static inline uint32_t ntohl(uint32_t w) { return NTOHL(w); }
static inline uint32_t htonl(uint32_t w) { return HTONL(w); }

// FIXME: add checksum computations for headers.

// http://www.microhowto.info/howto/calculate_an_internet_protocol_checksum_in_c.html
uint16_t ip_checksum(const void *vdata, size_t length) {

    const uint8_t *data = vdata;
    // Initialise the accumulator.
    uint32_t acc = 0xffff;
    // Handle complete 16-bit blocks.
    for (size_t i = 0; i+1 < length; i += 2) {
        uint16_t word;
        memcpy(&word, data + i, 2);
        acc += ntohs(word);
        if (acc > 0xffff) {
            acc -= 0xffff;
        }
    }
    // Handle any partial block at the end of the data.
    if (length & 1) {
        uint16_t word = 0;
        memcpy(&word, data + length - 1, 1);
        acc += ntohs(word);
        if (acc > 0xffff) {
            acc -= 0xffff;
        }
    }
    // Return the checksum in network byte order.
    return htons(~acc);

}

struct __attribute__((packed)) mac {
    uint8_t d_mac[6];
    uint8_t s_mac[6];
    uint16_t ethertype;
    // uint32_t checksum follows payload
} ;
struct __attribute__((packed)) ip {
    uint8_t version_ihl;
    uint8_t dscp_ecn;
    uint16_t total_length;
    uint16_t identification;
    uint16_t flags_fo;
    uint8_t ttl;
    uint8_t protocol;
    uint16_t header_checksum;
    uint8_t s_ip[4];
    uint8_t d_ip[4];
    // options if IHL>5
} ;
struct __attribute__((packed)) udp {
    uint16_t s_port;
    uint16_t d_port;
    uint16_t length;
    uint16_t checksum;
};

struct __attribute__((packed)) miu {
    struct mac m;
    struct ip  i;
    struct udp u;
};

struct __attribute__((packed)) msg {
    uint32_t len;
    struct miu miu;
    uint8_t data[2];
};

static struct msg heartbeat_template = {
    .miu = {
        .m = {
            .d_mac = {0x00, 0x1b, 0x21, 0xb3, 0x3d, 0x2c},
            .s_mac = {0xAE, 0, 0, 0, 0, 1},
            .ethertype = HTONS(0x0800), // IPv4
        },
        .i = {
            .version_ihl = 0x45,
            .dscp_ecn = 0,
            .total_length = HTONS(28+2),
            .identification = HTONS(0x1234),
            .flags_fo = HTONS(0x4000), // don't fragment
            .ttl = 0x40,
            .protocol = 0x11, // UDP,
            .header_checksum = 0,
            .s_ip = {10,1,3,123},
            .d_ip = {10,1,3,2}
        },
        .u = {
            .s_port = HTONS(54321),
            .d_port = HTONS(12345),
            .length = HTONS(8 + 2),
            .checksum = 0,
        },
    },
    .data = {'!','\n'}
};

void poll(void) {

    static uint32_t last_sec = 0;
    if (sec > last_sec) {
        last_sec++;
        struct msg h = heartbeat_template;
        h.miu.i.header_checksum = ip_checksum(&h.miu.i, 20);
        // h.miu.u.checksum = ip_checksum(&h.miu.i, xxx);
        h.len = HTONL(sizeof(h) - 4);
        cbuf_write(&outgoing, (const uint8_t*)&h, sizeof(h));
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






