/* OS example.  Main goal here is to combine existing SLIP protocol
   with interrupt -> cbuf and CSP wakeup in main loop. */

#include "base.h"
#include "gdbstub_api.h"
#include <string.h>

#include "sliplib.h"

#include "pbuf.h"
#include "cbuf.h"

#define TX_EN_OUT   GPIOA,8

static inline void hw_tx_dis(void) {
    hw_gpio_write(TX_EN_OUT,0);
}
static inline void hw_tx_en(void) {
    hw_gpio_write(TX_EN_OUT,1);
}


/* Incoming SLIP packets are decoded into flat packet, and
   app_write_end will be called when packet is ready. */

struct pbuf packet_in; uint8_t packet_in_buf[1024 + 64];
struct cbuf usb_out;   uint8_t usb_out_buf[16];
struct cbuf ser_out;   uint8_t ser_out_buf[64];
struct cbuf ser_in;    uint8_t ser_in_buf[64];

void app_write_byte(struct slip_write_state *s, uint8_t byte) {
    pbuf_put(&packet_in, byte);
}
void app_write_end(struct slip_write_state *s) {
    struct pbuf *p = &packet_in;

    if (p->count >= 2) {
        uint16_t tag = read_be(&p->buf[0], 2);
        switch(tag) {
        case TAG_PING:
            //infof("ping:%d\n",p->count-2);
            cbuf_write_slip_tagged(&usb_out, TAG_REPLY,
                                   &p->buf[2], p->count-2);
            break;
        case TAG_GDB:
            // infof("write TAG_GDB %d\n", p->count-2);
            _service.rsp_io.write(&p->buf[2], p->count-2);
            break;
        case 0xFF00:
            infof("write %d bytes to uart\n", p->count-2);
            cbuf_write(&ser_out, &p->buf[2], p->count-2);
            hw_usart_enable_send_ready_interrupt(USART1);
            break;
        case 0xFF01:
            infof("tx enable %d\n", p->buf[2]);
            if (p->buf[2]) {
                hw_tx_en();
            }
            else {
                hw_tx_dis();
            }
            break;
        default:
            break;
        }
    }
    p->count = 0;
}

static uint32_t ser_in_read(uint8_t *buf, uint32_t room) {
    return cbuf_read(&ser_in, buf, room);
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
    if ((rv = cbuf_read(&usb_out, buf, room))) return rv; // drain this first
    if ((rv = slip_read_tagged(0xFF01, ser_in_read, buf, room))) return rv;
    if ((rv = slip_read_tagged(TAG_INFO, info_read, buf, room))) return rv;
    if ((rv = slip_read_tagged(TAG_GDB, _service.rsp_io.read, buf, room))) return rv;
    return 0;
}

const struct gdbstub_io app_io = {
    .read  = app_read,
    .write = app_write,
};

static void switch_protocol(const uint8_t *buf, uint32_t size) {
    infof("os.c: SLIP on serial port.\n");
    *_service.io = (struct gdbstub_io *)(&app_io);
    (*_service.io)->write(buf, size);
}




/* Receive DMX on UART and convert to simple SLIP (Break=SLIP_END). */
/* Call by interrupt system when UART byte arrives. */
void usart1_isr(void) {
    infof("INT\n");

    /* Receive is independent, so handle it.  Interrupt flag is
       removed after DR read. */
    if (hw_usart1_recv_ready()) {
        int rv = hw_usart1_getchar_nsr();
        infof("RX:%d\n", rv);
        cbuf_put(&ser_in, rv);
    }

    /* There are two transmit interrupts handled here:
       TXE  Transmit register ready
       TC   Transmission done

       We use the interrupt enabled flag to check if we are at the end
       of a transmission. */

    if (hw_usart_send_ready_interrupt_enabled(USART1) &&
        hw_usart1_send_ready()) {
        /* Ack is done by write to DR.  There are two cases: */
        uint16_t token = cbuf_get(&ser_out);
        if (token != CBUF_EAGAIN) {
            /* either send the byte if buffer has more. */
            infof("TX:%d\n", token);
            hw_tx_en();
            hw_usart1_send(token);
        }
        else {
            /* or start end of transmission sequence. */
            infof("TX:done\n");
            hw_usart_disable_send_ready_interrupt(USART1);
            hw_usart_enable_send_done_interrupt(USART1);
        }
    }

    if (hw_usart_send_done_interrupt_enabled(USART1) &&
        hw_usart1_send_done()) {
        hw_usart_disable_send_done_interrupt(USART1);
        /* Turn off transmitter. */
        infof("TX:off\n");
        hw_tx_dis();
    }

}


void start(void) {
    /* Low level application init.  Note that this needs to be called
     * manually after loading to initialize memory. */
    hw_app_init();
    /* Buffer init.  Interrupts might use these, so init before
     * initializing interrupt hardware. */
    CBUF_INIT(usb_out);
    CBUF_INIT(ser_out);
    CBUF_INIT(ser_in);
    PBUF_INIT(packet_in);
    /* Hardware init. */

    // gpio for direction control
    rcc_periph_clock_enable(RCC_GPIOA | RCC_AFIO);
    hw_gpio_config(TX_EN_OUT,HW_GPIO_CONFIG_OUTPUT);
    hw_tx_dis();

    hw_usart1_init();
    hw_usart1_config(625, 1); // 115k2, receive interrupt enabled.
}

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "CSP demo";
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



