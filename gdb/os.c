/* OS example.  Main goal here is to combine existing SLIP protocol
   with interrupt -> cbuf and CSP wakeup in main loop. */

#include "base.h"
#include "gdbstub_api.h"
#include <string.h>

#include "sliplib.h"

#include "pbuf.h"
#include "cbuf.h"

#include "csp.h"

#define TX_EN_OUT   GPIOA,8

static inline void hw_tx_dis(void) {
    hw_gpio_write(TX_EN_OUT,0);
}
static inline void hw_tx_en(void) {
    hw_gpio_write(TX_EN_OUT,1);
}


struct pbuf packet_in; uint8_t packet_in_buf[1024 + 64];
struct cbuf usb_out;   uint8_t usb_out_buf[16];
struct cbuf ser_out;   uint8_t ser_out_buf[64];
struct cbuf ser_in;    uint8_t ser_in_buf[64];
struct cbuf hw_event;  uint8_t hw_event_buf[16];

/* Interrupts are translated to a single event queue. */
#define HW_EVENT_RX 0



/* Tasks & Channels */

#define CHAN_USB_IN   0
#define CHAN_HW_EVENT 1
#define NB_CHAN 2

struct input_task {
    struct csp_task task;
    struct csp_evt evt[2];
    void *next;
    uint8_t msg[8];
} input_task;
void input_resume(struct input_task *e) {
    if(e->next) goto *e->next;
  again:
    CSP_EVT(e, 0, CHAN_USB_IN,   e->msg);
    CSP_EVT(e, 1, CHAN_HW_EVENT, e->msg);
    CSP_SEL(e, 0/*nb_send*/, 2/*nb_recv*/);
    switch(e->task.selected) {
    case 0:
        infof("input RCV: %d\n", e->evt[0].msg_len);
        goto again;
    case 1:
        infof("HW_EVENT %d\n", e->msg[0]);
        goto again;
    }
}



// FIXME: Properly set scheduler data structure sizes.
struct csp_scheduler   sched;
struct csp_evt_list    sched_c2e[NB_CHAN*10]; // FIXME
struct csp_chan_to_evt sched_c[NB_CHAN];


static inline void sched_init(void) {
    csp_scheduler_init(
        &sched,
        sched_c2e, ARRAY_SIZE(sched_c2e),
        sched_c,   ARRAY_SIZE(sched_c));
    input_task.task.resume = (csp_resume_f)input_resume;
    csp_start(&sched, &input_task.task);
}


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
            /* Anything else goes into the CSP network.  We won't
             * buffer any further.  A task needs to be waiting for the
             * message. */
            if (!csp_send(&sched, CHAN_USB_IN, &p->buf[0], p->count)) {
                infof("CSP USB message dropped: %d bytes\n", p->count);
            }
            break;
        }
    }
    p->count = 0;
}

/* FIXME: Only poll on interrupt. */
static void poll_event(void) {
    uint16_t token = cbuf_get(&hw_event);
    if (CBUF_EAGAIN != token) {
        if (!csp_send(&sched, CHAN_HW_EVENT, &token, sizeof(token))) {
            infof("CSP hw_event dropped\n");
        }
    }
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
#define DBG(...)
//#define DBG(...) infof(__VA_ARGS__)
void usart1_isr(void) {
    DBG("INT\n");

    /* Receive is independent, so handle it.  Interrupt flag is
       removed after DR read. */
    if (hw_usart1_recv_ready()) {
        int rv = hw_usart1_getchar_nsr();
        DBG("RX:%d\n", rv);
        cbuf_put(&hw_event, rv);
    }

    /* There are two transmit interrupts handled here:
       TXE  Transmit register ready
       TC   Transmission done

       We use the interrupt enabled flag to check if we are at the end
       of a transmission. */

    if (hw_usart_send_ready_interrupt_enabled(USART1) &&
        hw_usart1_send_ready()) {
        uint16_t token = cbuf_get(&ser_out);
        if (token != CBUF_EAGAIN) {
            DBG("TX:%d\n", token);
            hw_tx_en();
            hw_usart1_send(token);  // This acks, leave interrupt on
        }
        else {
            DBG("TX:flush\n");
            hw_usart_disable_send_ready_interrupt(USART1);
            hw_usart_enable_send_done_interrupt(USART1);
        }
    }

    if (hw_usart_send_done_interrupt_enabled(USART1) &&
        hw_usart1_send_done()) {
        hw_usart_disable_send_done_interrupt(USART1);
        DBG("TX:off\n");
        hw_tx_dis();
    }

}
#undef DBG


void start(void) {
    /* Low level application init.  Note that this needs to be called
     * manually after loading to initialize memory. */
    hw_app_init();
    /* Buffer init.  Interrupts might use these, so init before
     * initializing interrupt hardware. */
    CBUF_INIT(usb_out);
    CBUF_INIT(ser_out);
    CBUF_INIT(ser_in);
    CBUF_INIT(hw_event);
    PBUF_INIT(packet_in);

    /* CSP scheduler init. */
    sched_init();

    /* Hardware init. */

    // gpio for direction control
    rcc_periph_clock_enable(RCC_GPIOA | RCC_AFIO);
    hw_gpio_config(TX_EN_OUT,HW_GPIO_CONFIG_OUTPUT);
    hw_tx_dis();

    hw_usart1_init();
    hw_usart1_config(625, 1); // 115k2, receive interrupt enabled.

    /* Polling routine: FIXME: This can go in a WFI loop. */
    _service.add(poll_event);

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



