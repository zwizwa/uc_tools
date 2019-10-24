/* Stand alone "lab board".

   This started out as a relay controller, but will likely turn into a
   kitchen sink for misc dev lab related I/O.

   See relay_board.c for a simpler example.

   This version uses a slip-wrapped serial port for increased
   robustness.

   Note that for USB, slip is only necessary if we don't fully control
   the bringup of the port.  In this case (bluepill.erl) we might be
   connecting to a board that has already been running, and that has a
   receiver that is in an unknown state.  Send it an empty packet to
   sync.

   STM32F103 pin config for UART1
   A9  = TX
   A10 = RX  (5V tolerant on STM32F103x8)

   Wish list:
   - bit-bang routines for different protocols (SPI, MII, ...)
   - programmable bit-bang?

*/

#include "base.h"
#include "gdbstub_api.h"
#include <string.h>

#include "cbuf.h"
#include "pbuf.h"


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

struct pbuf packet_in; uint8_t packet_in_buf[2+4+1024]; // FLASH_WRITE


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





/* Message tags.  See also slip.h */
#define TAG_SET_PIN     0x0000
#define TAG_STATUS      0x0001


/* Handle I/O command.  Routing tag is still present in the buffer. */
static void command_io(const struct pbuf *p) {
    /* This doesn't need to be complicated.  Support 26 I/O lines,
       where capital letter corresponds to high and lower caps is
       low. */
    switch(p->buf[2]) {
    case 'A': set_pin(3,1); break;
    case 'B': set_pin(4,1); break;
    case 'C': set_pin(5,1); break;
    case 'D': set_pin(6,1); break;
    case 'a': set_pin(3,0); break;
    case 'b': set_pin(4,0); break;
    case 'c': set_pin(5,0); break;
    case 'd': set_pin(6,0); break;
    }
    /* Caller supplies ack message so we can be dumb here and just
       echo it back.  This allows for CPS-style synchronization. */
    cbuf_write_slip_tagged(&slip_out, TAG_REPLY,
                           &p->buf[3], p->count-3);
}


#include "plugin_api.h"

void dispatch(void *ctx, const struct pbuf *p) {
    if (p->count < 2) return;
    uint16_t tag = read_be(p->buf, 2);

    if (plugin_write_message(p->buf, p->count)) return;

    switch(tag) {
    case TAG_PING:
        //infof("ping:%d\n",p->count-2);
        cbuf_write_slip_tagged(&slip_out, TAG_REPLY,
                               &p->buf[2], p->count-2);
        break;
    case TAG_SET_PIN:
        if (p->count >= 3) command_io(p);
        break;
    case TAG_GDB:
        // infof("tag_gdb: %d\n", p->count);
        _service.rsp_io.write(&p->buf[2], p->count-2);
    case TAG_UART:
        //infof("tag_uart: %d\n", p->count);
        cbuf_write(&uart1_out, &p->buf[2], p->count-2);
        break;
    default:
        infof("unknown message: tag=%02x len=%d\n", tag, p->count);
        break;
    }

}


int poll_status(struct cbuf *b) {
    if (tick != tick_last) {
        // FIXME: Don't send out status info unless someone is
        // actually listening.  Uart will be buffered (indefinitely?)
        // by linux, which creates a flood on opening.

        // cbuf_write_slip_tagged(b, TAG_STATUS, (void*)&tick_last, 4);
        tick_last++;
        return 1;
    }
    else {
        return 0;
    }
}

int poll_read(struct cbuf *b, uint16_t tag,
              uint32_t (*read)(uint8_t *buf, uint32_t len)) {
    uint8_t buf[40]; // What's a good size?
    uint32_t n = read(buf, sizeof(buf));
    if (!n) return 0;
    cbuf_write_slip_tagged(b, tag, buf, n);
    return 1;
}
static uint32_t uart1_read(uint8_t *buf, uint32_t len) {
    return cbuf_read(&uart1_in, buf, len);
}

void poll_machines(struct cbuf *b) {
    if (poll_status(b)) return;
    if (poll_read(b, TAG_INFO, info_read)) return;
    if (poll_read(b, TAG_GDB, _service.rsp_io.read)) return;
    if (poll_read(b, TAG_UART, uart1_read)) return;
    if (poll_read(b, TAG_PLUGIO, plugin_read)) return;
}

// Poll independent of read
static void poll_uart1_tx(void) {
    if (!hw_usart1_send_ready()) return;
    uint16_t fc = cbuf_get(&uart1_out);
    if (CBUF_EAGAIN == fc) return;
    hw_usart1_send(fc & 0xFF);
}

static uint32_t slip_read(uint8_t *buf, uint32_t room) {

    /* Flush old */
    uint32_t nb = cbuf_read(&slip_out, buf, room);
    buf += nb;  room -= nb;
    if (!room) return nb;

    /* There is room for more, so poll each of the state machines
     * according to their priority to generate more data, and do
     * another flush up to available space.  Lazily generating
     * messages like this effectively creates backpressure.  It's
     * often a lot more efficient to let machines hold on to state
     * that can produce a new message, than it is to have them dump
     * serialized data into a buffer at an earlier stage. */
    poll_machines(&slip_out);
    return nb + cbuf_read(&slip_out, buf, room);
}


/* We don't have flow control here.  The sending end should use the
   ack mechanism to avoid overflows. */
static void slip_write(const uint8_t *buf, uint32_t len) {
    pbuf_slip_write(
        buf, len,
        &slip_in, &packet_in,
        dispatch, NULL);
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


void start(void) {
    /* Low level application init */
    hw_app_init();

    /* IO init */
    rcc_periph_clock_enable(RCC_GPIOA | RCC_AFIO);

    /* Data structure init */
    CBUF_INIT(uart1_in);
    CBUF_INIT(uart1_out);
    CBUF_INIT(slip_in);
    CBUF_INIT(slip_out);
    PBUF_INIT(packet_in);

    hw_periodic_init(C_PERIODIC);
    usart1_init();

    _service.add(poll_uart1_tx);

    infof("lab_board.c\n");
    infof("_eflash = 0x%08x\n", &_eflash);
    infof("_ebss   = 0x%08x\n", &_ebss);

}
void stop(void) {
    hw_app_stop();
    _service.reset();
}

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "Lab Board";
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



