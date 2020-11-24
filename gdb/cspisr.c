/* Test for a single-priority csp-based interrupt driven state machine architecture.
   There are 3 priority levels:

   hw_isr: moves hardware events to ring buffer, triggers software sw_isr
   sw_isr: polls hw event ring buffer, pushes into csp network
   main:   handle non-isr events

   The ringbuffer is there to handle the case where the CSP network is
   not fast enough to handle multiple interrupts before buffers run
   out.
*/

#include "base.h"
#include "gdbstub_api.h"
#include "cbuf.h"
#include "pbuf.h"
#include "sliplib.h"
#include <string.h>

#define TICKS_PER_US 1

const struct hw_delay hw_tim[] = {
//          rcc       irq            tim   psc
//-------------------------------------------------------
    [3] = { RCC_TIM3, NVIC_TIM3_IRQ, TIM3, 72*TICKS_PER_US },
};
#define TIM 3
#define C_TIM hw_tim[TIM]

volatile struct {
    uint32_t tim_isr_count;
} stat;

struct pbuf packet_in; uint8_t packet_in_buf[1024 + 64];
struct cbuf usb_out;   uint8_t usb_out_buf[16];
struct cbuf hw_event;  uint8_t hw_event_buf[16];
#define HW_EVENT_TIMEOUT 1

/* Timeout interrupt. */
void HW_TIM_ISR(TIM)(void) {
    stat.tim_isr_count++;
    cbuf_put(&hw_event, HW_EVENT_TIMEOUT);
    hw_delay_ack(C_TIM);
}

static void poll_event(void) {
    uint16_t token;
    if (CBUF_EAGAIN != (token = cbuf_get(&hw_event))) {
        infof("event\n");
        //if (!csp_send(&sched, CHAN_HW_EVENT, &token, sizeof(token))) {
        //    infof("CSP hw_event dropped\n");
        //}
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
        //if (!csp_send(&sched, CHAN_USB_IN, &p->buf[0], p->count)) {
        //    infof("CSP USB message dropped: %d bytes\n", p->count);
        //}
        break;
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
void start(void) {
    hw_app_init();
    CBUF_INIT(hw_event);
    CBUF_INIT(usb_out);
    PBUF_INIT(packet_in);
    hw_delay_init(C_TIM, 0xFFFF, 1 /*enable interrupt*/);
    _service.add(poll_event);
    infof("cspisr\n");

}

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "CSP ISR Test";
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;
const char config_version[]      CONFIG_DATA_SECTION = "current";
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
