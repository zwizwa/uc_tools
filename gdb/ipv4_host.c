/* Stand alone host.c clone that talks over ipv4 using standard
   "slattach" slip. */

// FIXME: Turn this into a generic wrapper that tunnels the normal
// protocol over udp.

#include "base.h"
#include "gdbstub_api.h"
#include <string.h>

#include "cbuf.h"
#include "pbuf.h"

#include "reset_device.h"

#include "crc.h"

/* Buffering.

   There is plenty of space, so use it.  Below is the general
   structure of buffers and state machines.

   -> data motion
   [] buffer
   {} stateless function
   <> state machine

   USB -> {slip_in} -> [slip_in] -> {poll_slip_in} -> [packet_in]


   The output chain consists of two mechanisms: some incoming packet
   handlers are simpler to write if they can queue a return packet.
   For anything else, state is managed locally in some more efficient
   encoding, and a message is only created when necessary.  Note that
   this means that direct replies always have priority.

*/

/* RPC calls.

   To properly associate replies to requests, allow the caller to
   provide an opaque reference, and just echo it back.  In Erlang on a
   trusted line, this can even be a continuation closure.  On an
   untrusted line, add a MAC & timestamp.
*/


struct cbuf slip_in;  uint8_t slip_in_buf[4];
struct cbuf slip_out; uint8_t slip_out_buf[256*2];

struct cbuf uart1_in;  uint8_t uart1_in_buf[1024];
struct cbuf uart1_out; uint8_t uart1_out_buf[1024];

struct pbuf packet_in; uint8_t packet_in_buf[64+1024]; // FLASH_WRITE


/* When device receives anything other than ipv4 or ipv6 it will
   switch to tethered mode. */
int tethered = 0;


// Main rate is just base clock for audio
#define DIV 1500 // (/ 72000000 48000)
#define TIM_PERIODIC 4
static volatile uint32_t sample;
static volatile uint32_t tick;
uint32_t tick_last;
uint32_t tick_period = 48000;

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

void usart1_isr(void) {
    /* Get SR + DR from the UART. Low 8 bits are received data, the 4
     * bits above that are status register (SR) bits [ORE NE FE PE] */
    /* ACK is done by read. */
    int rv = hw_usart1_getchar_nsr();
    uint32_t sr_dr = rv & 0xFFF;
    /* FIXME: handle error conditions */
    cbuf_put(&uart1_in, sr_dr & 0xFF);
}
static void usart1_init(void) {
    // uart controller init, io port setup
    hw_usart1_init();
    // uart config.
    hw_usart_disable(USART1);
    hw_usart_set_databits(USART1, 8);
    hw_usart_set_stopbits(USART1, USART_STOPBITS_1);
    hw_usart_set_mode(USART1, USART_MODE_TX_RX);
    hw_usart_set_parity(USART1, USART_PARITY_NONE);
    hw_usart_set_flow_control(USART1, USART_FLOWCONTROL_NONE);
    USART_BRR(USART1) = 72000000 / 115200;
    hw_usart_enable_rx_interrupt(USART1);
    hw_usart_enable(USART1);
}


KEEP void set_pin(int pin, int val) {
    hw_gpio_write(GPIOA,pin,val);
    hw_gpio_config(GPIOA,pin,HW_GPIO_CONFIG_OUTPUT);
}







// Multicast/broadcast seems most convenient.
// https://networklessons.com/multicast/multicast-ip-address-to-mac-address-mapping
// socat - UDP4-RECVFROM:1234,ip-add-membership=224.0.13.1:10.1.3.2,fork | hd
// socat - UDP4-RECVFROM:1234,fork | hd

#if 0
// multicast
#define MULTICAST 0,13,1
#define DIP 224,MULTICAST
#endif

#if 0
// broadcast
#define DIP 255,255,255,255
#endif

#if 1
// gateway
#define DIP 10,5,0,1
#endif

#include "ethernet.h"

struct __attribute__((packed)) headers {
    struct ip  i;
    struct udp u;
};
uint8_t sip[4];

#define HEADERS_INIT  {                                 \
    .i = {                                              \
        .version_ihl = 0x45,                            \
        .dscp_ecn = 0,                                  \
        .identification = HTONS(0x1234), /* ?? */       \
        .flags_fo = HTONS(0x4000), /* don't fragment */ \
        .ttl = 0x40,                                    \
        .protocol = 0x11, /* UDP */                     \
        .d_ip = {DIP}                                   \
    },                                                  \
    .u = {                                              \
        .s_port = HTONS(4321),                          \
        .d_port = HTONS(1234),                          \
    }                                                   \
}

void init_sip_from_device_id(uint8_t *ip) {
    const uint8_t *stm_id = (void*)0x1FFFF7E8;
    uint32_t crc = crc32b(stm_id, 12);
    // modulo and offset to leave out network address (0), gateway
    // (1), and broadcast (-1).
    uint32_t id = 2 + (crc % (0x10000 - 3));
    ip[0] = 10;
    ip[1] = 5;
    ip[2] = id >> 8;
    ip[3] = id;
    infof("ip: %d.%d.%d.%d\n", ip[0], ip[1], ip[2], ip[3]);
}



static void send_udp(const uint8_t *buf, uint32_t len) {
    /* Do not use infof in this dynamic extent to avoid a feedback
     * loop.  Logs are sent out over udp. */

    struct headers h = HEADERS_INIT;
    memcpy(h.i.s_ip, sip, 4);

    h.i.total_length = htons(28 + len);
    h.i.header_checksum = ip_checksum(&h.i, sizeof(h.i));
    h.u.length   = htons( 8 + len);
    h.u.checksum = 0;

    // For ipv4 we use standard slip, where ip header provides the tag
    // which is in range 0x4000 0x4fff
    const struct slice slices[] = {
        {.buf = (uint8_t*)&h, sizeof(h) },
        {.buf = buf, len },
    };
    cbuf_write_slip_slices(&slip_out, slices, 2);
}


#include "plugin_api.h"

/* gdbstub_hub tethered messages. */
void dispatch_tethered(void *ctx, const struct pbuf *p) {
    if (p->count < 2) return;
    uint16_t tag = read_be(p->buf, 2);

    /* The packet_tags.h are still supported as well. */

    if (plugin_handle_message(p->buf, p->count)) return;

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
        // infof("tag_gdb: %d\n", p->count);
        _service.rsp_io.write(&p->buf[2], p->count-2);
    case TAG_STREAM: {
        uint16_t stream_tag = read_be(p->buf+2, 2);
        //infof("tag_uart: %d\n", p->count);
        if (0 == stream_tag) {
            cbuf_write(&uart1_out, &p->buf[4], p->count-4);
        }
        else {
            /* Unknown stream. */
        }
        break;
    }
    default:
        infof("unknown message: tag=%02x len=%d\n", tag, p->count);
        break;
    }
}

void dispatch_ipv4(void *ctx, const struct pbuf *p) {
    infof("ipv4: %d bytes\n", p->count);
}

void dispatch_ipv6(void *ctx, const struct pbuf *p) {
    infof("ipv6: %d bytes\n", p->count);
}


void info_firmware(void);
void set_tethered(int mode) {
    if (mode) {
        if (!tethered) {
            infof("switching to tethered mode\n");
            info_firmware();
            tethered = 1;
        }
    }
    else {
        if (tethered) {
            infof("switching to ipv4 mode\n");
            info_firmware();
            tethered = 0;
        }
    }
}

/* Any protocol. */
void dispatch_any(void *ctx, const struct pbuf *p) {
    if (p->count < 2) return;
    uint16_t tag = read_be(p->buf, 2);

    /* The main interface of this board is slattach compatible ipv4 /
       ipv6 that maps to an ip tunnel tun device on linux (tun, not
       tap). */
    if ((tag & 0xF000) == 0x4000) {
        set_tethered(0);
        dispatch_ipv4(ctx, p);
        return;
    }
    if ((tag & 0xF000) == 0x6000) {
        set_tethered(0);
        dispatch_ipv6(ctx, p);
        return;
    }
    set_tethered(1);
    dispatch_tethered(ctx, p);
}


void poll_tick(void) {
    if (tick == tick_last) return;
    tick_last++;

    infof("tick\n");
    //uint32_t payload = 123;
    //send_udp((uint8_t*)&payload, sizeof(payload));
}

int poll_read(struct cbuf *b, uint16_t tag,
              uint32_t (*read)(uint8_t *buf, uint32_t len)) {
    uint8_t buf[40]; // What's a good size?
    uint32_t n = read(buf, sizeof(buf));
    if (!n) return 0;
    cbuf_write_slip_tagged(b, tag, buf, n);
    return 1;
}
uint32_t uart1_read(uint8_t *buf, uint32_t len) {
    return cbuf_read(&uart1_in, buf, len);
}

void poll_machines(struct cbuf *b) {
    if (poll_read(b, TAG_INFO, info_read)) return;
    if (poll_read(b, TAG_GDB, _service.rsp_io.read)) return;
    //if (poll_read(b, TAG_UART, uart1_read)) return;
    if (poll_read(b, TAG_PLUGIO, plugin_read)) return;
}

// Poll independent of read
static void poll_uart1_tx(void) {
    if (!hw_usart1_send_ready()) return;
    uint16_t fc = cbuf_get(&uart1_out);
    if (CBUF_EAGAIN == fc) return;
    hw_usart1_send(fc & 0xFF);
}

static void poll_events(void) {

    /* In tethered mode we can just send it out as a TAG_INFO
       packet. */
    if (tethered) return;

    /* In the untethered case we only send logs.  FIXME: add some kind
       of tagging here, or use a different port. */
    if (cbuf_room(&slip_out) > 300) { // FIXME: tighten bounds
        uint8_t buf[100];
        uint32_t n = info_read(buf, sizeof(buf));
        if (n) send_udp(buf, n);
    }
}


static uint32_t slip_read(uint8_t *buf, uint32_t room) {

    /* Flush old */
    uint32_t nb = cbuf_read(&slip_out, buf, room);
    buf += nb;  room -= nb;
    if (!room) return nb;

    /* Only send out other tags in tethered mode. */
    if (!tethered) return nb;

    /* There is room for more, so poll each of the state machines
     * according to their priority to generate more data, and do
     * another flush up to available space.  Lazily generating
     * messages like this effectively creates backpressure.  It's
     * often a lot more efficient to let machines hold on to state
     * that can produce a new message, than it is to have them
     * dump serialized data into a buffer at an earlier stage. */
    poll_machines(&slip_out);
    return nb + cbuf_read(&slip_out, buf, room);
}


/* We don't have flow control here.  The sending end should use the
   ack mechanism to avoid overflows. */
static void slip_write(const uint8_t *buf, uint32_t len) {
    pbuf_slip_write(
        buf, len,
        &slip_in, &packet_in,
        dispatch_any, NULL);
}
const struct gdbstub_io slip_io = {
    .read  = slip_read,
    .write = slip_write,
};



// BOILERPLATE


/* If bootloader sees a message that does not parse as GDB RSP, it
   passes it here so we can install a new i/o handler on the virtual
   serial port. */
void switch_protocol(const uint8_t *buf, uint32_t size) {
    *_service.io = (struct gdbstub_io *)(&slip_io);
    (*_service.io)->write(buf, size);
}



void info_firmware(void);

void start(void) {
    /* Low level application init */
    hw_app_init();

    /* IO init */
    rcc_periph_clock_enable(RCC_GPIOA | RCC_GPIOB | RCC_GPIOC | RCC_AFIO);

    /* Data structure init */
    CBUF_INIT(uart1_in);
    CBUF_INIT(uart1_out);
    CBUF_INIT(slip_in);
    CBUF_INIT(slip_out);
    PBUF_INIT(packet_in);

    init_sip_from_device_id(sip);

    hw_periodic_init(C_PERIODIC);
    usart1_init();

    _service.add(poll_uart1_tx);
    _service.add(poll_tick);
    _service.add(poll_events);

    info_firmware();

}
void stop(void) {
    hw_app_stop();
    _service.reset();
}

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "SLIP IP test";
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;
const char config_version[]      CONFIG_DATA_SECTION = BUILD;
const char config_protocol[]     CONFIG_DATA_SECTION = "{driver,lab_board,slip}";

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

void info_firmware(void) {
    infof("product: %s\n", config_product);
    infof("_eflash = 0x%08x\n", &_eflash);
    infof("_ebss   = 0x%08x\n", &_ebss);
}


