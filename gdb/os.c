/* Explore some reusable abstractions once CSP scheduler is part of
   the picture.  Might evolve into more traditional OS.

   Main goal here is to create some hardware abstractions that
   communicate with CSP network through protocols instead of functions
   and callbacks.  This should make it a lot easier to move logic
   between bare metal and Linux processes. */

#include "base.h"
#include "gdbstub_api.h"
#include <string.h>

#include "pbuf.h"
#include "cbuf.h"

#include "sliplib.h"

#include "csp.h"

#include "crc.h"

#define TX_EN_OUT   GPIOA,8

static inline void hw_tx_dis(void) {
    hw_gpio_write(TX_EN_OUT,0);
}
static inline void hw_tx_en(void) {
    hw_gpio_write(TX_EN_OUT,1);
}

static void log_buf(const uint8_t *buf, uint32_t len) {
    for(int i=0; i<len; i++) {
        infof(" %02x", buf[i]);
    }
    infof("\n");
}


/* For timeouts.  Prescaler is a 16 bit number that divides the 72MHz
 * clock.  Timescales:
 * 72   -> 1us
 * 720  -> 10us
 * 7200 -> 100us
 */
#define TICKS_PER_US 10

const struct hw_delay hw_tim[] = {
//          rcc       irq            tim   psc
//-------------------------------------------------------
    [2] = { RCC_TIM2, NVIC_TIM2_IRQ, TIM2, 72*TICKS_PER_US },
};
#define TIM 2
#define C_TIM hw_tim[TIM]

static inline void hw_set_recv_timeout_us(uint32_t us) {
    uint32_t ticks = (us / TICKS_PER_US) & 0xFFFF;
    infof("ticks %d\n", ticks);
    hw_delay_arm(C_TIM, ticks);
    hw_delay_trigger(C_TIM);
}



struct pbuf packet_in; uint8_t packet_in_buf[1024 + 64];
struct cbuf usb_out;   uint8_t usb_out_buf[16];
struct cbuf ser_out;   uint8_t ser_out_buf[64];
struct cbuf hw_event;  uint8_t hw_event_buf[16];


uint32_t ser_buf = 0;
struct pbuf ser_in[2];
uint8_t ser_in_buf[128];


/* Interrupts are translated to a single event queue. */
#define HW_EVENT_RX_PBUF0 0
#define HW_EVENT_RX_PBUF1 1
#define HW_EVENT_TIMEOUT 255




/* Tasks & Channels */

#define CHAN_USB_IN   0
#define CHAN_HW_EVENT 1
#define NB_CHAN 2


/* Dispatch events to a variety of channels.  This task is used to
   handle external network input.  The idea is that we are always
   ready to receive a message, hence the sends performed here should
   also finish as part of a single scheduler run so we can accept the
   next message. */
struct input_task {
    struct csp_task task;
    struct csp_evt evt[2];
    void *next;
    uint8_t msg[16];
} input_task;
csp_status_t input_resume(struct input_task *e) {
    if(e->next) goto *e->next;

    for(;;) {

        CSP_EVT(&(e->task), 0, CHAN_USB_IN,   e->msg);
        CSP_EVT(&(e->task), 1, CHAN_HW_EVENT, e->msg);
        CSP_SEL(&(e->task), e, 0/*nb_send*/, 2/*nb_recv*/);

        if (0 == e->task.selected) {
            /* CHAN_USB_IN: Message from USB Host. */
            uint32_t count = e->evt[0].msg_len;
            infof("USB_IN: %d\n", count);
            switch(read_be(e->msg, 2)) {
            case 0xFF00: {
                if (count < sizeof(e->msg)) {
                    e->msg[count] = crc8x_simple(0, &e->msg[2], count-2);
                    count++;
                }
                infof("write %d bytes to uart\n", count-2);
                log_buf(&e->msg[2], count-2);
                cbuf_write_slip(&ser_out, &e->msg[2], count-2);
                hw_usart_enable_send_ready_interrupt(USART1);
                break;
            }
            case 0xFF01:
                infof("tx enable %d\n", e->msg[2]);
                if (e->msg[2]) { hw_tx_en();  }
                else           { hw_tx_dis(); }
                break;
            case 0xFF02: {
                uint32_t us = read_be(e->msg+2, 4);
                infof("timeout %d us\n", us);
                hw_set_recv_timeout_us(us);
                break;
            }
            default:
                infof("unknown USB message %d\n", e->msg[0]);
                break;
            }
        }
        else {
            /* CHAN_HW_EVENT: Hardware event. */
            switch(e->msg[0]) {
            case HW_EVENT_RX_PBUF0:
            case HW_EVENT_RX_PBUF1: {
                uint32_t buf = e->msg[0] - HW_EVENT_RX_PBUF0;
                /* A complete message has been received on serial I/O */
                infof("HW_EVENT_RX_PBUF: %d %d\n", e->msg[0], ser_in[buf].count);
                uint8_t crc = crc8x_simple(0, ser_in[buf].buf, ser_in[buf].count-1);
                infof("crc %02x %02x\n", crc, ser_in[buf].buf[ser_in[buf].count-1]);
                log_buf(ser_in[buf].buf, ser_in[buf].count);
                break;
            }
            default:
                infof("unknown HW_EVENT %d\n", e->msg[0]);
                break;
            }
        }
    }
}



// FIXME: Properly set scheduler data structure sizes.
struct csp_scheduler   sched;
struct csp_evt_list    sched_e[NB_CHAN*2]; // FIXME: make this exact?
struct csp_chan_to_evt sched_c[NB_CHAN];


static inline void sched_init(void) {
    csp_scheduler_init(
        &sched,
        sched_e, ARRAY_SIZE(sched_e),
        sched_c, ARRAY_SIZE(sched_c));
    input_task.task.resume = (csp_resume_f)input_resume;
    csp_start(&sched, &input_task.task);
}


/* FIXME: Only poll on interrupt. */
static void poll_event(void) {
    uint16_t token;
    if (CBUF_EAGAIN != (token = cbuf_get(&hw_event))) {
        if (!csp_send(&sched, CHAN_HW_EVENT, &token, sizeof(token))) {
            infof("CSP hw_event dropped\n");
        }
    }
}


void app_handle(struct pbuf *p) {
    if (!p->count) return;

    /* Keep non-CSP buffer drains out of CSP tasks, and move anything
       else into the CSP input task. */
    switch(read_be(&p->buf[0], 2)) {
    case TAG_PING:
        cbuf_write_slip_tagged(&usb_out, TAG_REPLY, &p->buf[2], p->count-2);
        break;
    case TAG_GDB:
        _service.rsp_io.write(&p->buf[2], p->count-2);
        break;
    default:
        if (!csp_send(&sched, CHAN_USB_IN, &p->buf[0], p->count)) {
            infof("CSP USB message dropped: %d bytes\n", p->count);
        }
    }
}
static void app_write(const uint8_t *buf, uint32_t len) {
    /* Incoming SLIP protocol goes to pbuf and is handled when complete. */
    pbuf_slip_for(&packet_in, buf, len, app_handle);
}
static uint32_t app_read(uint8_t *buf, uint32_t room) {
    int rv;
    if ((rv = cbuf_read(&usb_out, buf, room))) return rv; // drain this first
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



/* Timeout interrupt. */
volatile uint32_t tim_isr_count = 0;
void HW_TIM_ISR(TIM)(void) {
    tim_isr_count++;
    cbuf_put(&hw_event, HW_EVENT_TIMEOUT);
    hw_delay_ack(C_TIM);
}



/* Interrupt-driven UART I/O.  It is assumed packets are SLIP. */

#define DBG(...)
//#define DBG(...) infof(__VA_ARGS__)
void usart1_isr(void) {
    DBG("INT\n");

    /* Receive is independent, so handle it.  Interrupt flag is
       removed after DR read.  Interrupt is always enabled. */
    if (hw_usart1_recv_ready()) {
        int rv = hw_usart1_getchar_nsr();
        DBG("RX:%d\n", rv);
        if (SLIP_END == rv) {
            if (ser_in[ser_buf].count) {
                cbuf_put(&hw_event, HW_EVENT_RX_PBUF0+ser_buf);
                ser_buf^=1;
            }
            pbuf_slip_clear(&ser_in[ser_buf]);
        }
        else {
            pbuf_slip_put(&ser_in[ser_buf], rv);
        }
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
    CBUF_INIT(hw_event);
    PBUF_INIT(packet_in);

    /* Double buffered: one for ISR and one for CSP network. */
    pbuf_pool_init(&ser_in[0], ser_in_buf, 64, 2);

    /* CSP scheduler init. */
    sched_init();

    /* Hardware init. */

    // gpio for direction control
    rcc_periph_clock_enable(RCC_GPIOA | RCC_AFIO);
    hw_gpio_config(TX_EN_OUT,HW_GPIO_CONFIG_OUTPUT);
    hw_tx_dis();

    /* */
    hw_delay_init(C_TIM, 0xFFFF, 1 /*enable interrupt*/);

    /* FIXME: There is a bug in the timer init.  The first arm,trigger
     * sequence causes the ISR to fire immediately.  After that the
     * timeouts are correct.  I can't find the problem so this is a
     * workaround that triggers it once and clears the buffer. */
    hw_set_recv_timeout_us(10000);
    while(cbuf_empty(&hw_event)); // interrupt will add char
    cbuf_clear(&hw_event); // remove the char


    hw_usart1_init();
//#define BR 625 // 115k2
#define BR 16 //4.5M (max)

    hw_usart1_config(BR, 1);

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



