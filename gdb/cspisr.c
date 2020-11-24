/* Test for a single-priority csp-based interrupt driven state machine architecture.
   There are 3 priority levels:

   hw_isr: moves hardware events to ring buffer, triggers software sw_isr
   sw_isr: polls hw event ring buffer, pushes into csp network
   main:   handle non-isr events

   The ringbuffer is there to handle the case where the CSP network is
   not fast enough to handle multiple interrupts before buffers run
   out.

   The hw_isr is also useful for translating and filtering hardware
   events to perform data reduction.


   TODO: First make all interrups arrive in a single point.  3xUART
   RX/TX, 3xGPIO, timer.

*/

#include "base.h"

#include "gdbstub_api.h"
#include "cbuf.h"
#include "pbuf.h"
#include "sliplib.h"
#include "tag_u32.h"
#include "swtimer.h"

#include <stdint.h>
#include <string.h>


#define TICKS_PER_US 100

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

struct cbuf hw_event;  uint8_t hw_event_buf[16];
#define HW_EVENT_TIMEOUT 1

/* Timeout interrupt. */
swtimer_element_t swtimer_elements[10]; // FIXME: how many?
struct swtimer swtimer = { .nb = 0, .arr = swtimer_elements };
uint16_t swtimer_last = 0;

void HW_TIM_ISR(TIM)(void) {
    stat.tim_isr_count++;
    cbuf_put(&hw_event, HW_EVENT_TIMEOUT);
    hw_delay_ack(C_TIM);

    swtimer_element_t next;
    if (swtimer_next(&swtimer, &next)) {
        /* FIXME: How to keep the timer rolling?  Currently the update
           depends on the speed at which we can handle the interrupt,
           which isn't a huge problem for many applications but
           introduces subtle delays... */
        uint16_t diff = next.time - swtimer_last;
        swtimer_last = next.time;
        hw_delay_arm(C_TIM, diff);
        hw_delay_trigger(C_TIM);
    }
}

static void poll_event(void) {
    static uint32_t event;
    uint16_t token;
    if (CBUF_EAGAIN != (token = cbuf_get(&hw_event))) {
        infof("event %d\n", event++);
        //if (!csp_send(&sched, CHAN_HW_EVENT, &token, sizeof(token))) {
        //    infof("CSP hw_event dropped\n");
        //}
    }
}



/* Protocol-wise we don't really need anything special, so use
   slipstub_buffered to do the basics (SLIP framing, PING, GDB, INFO)
   and use tag_u32 for app-specific commands. */
#include "slipstub.h"
struct slipstub slipstub;
struct slipstub_buffers slipstub_buffers;


/* slipstub calls this one for application tags.  We then patch
   through to command handler. */

int handle_tag_u32(
    void *context,
    const uint32_t *arg,  uint32_t nb_args,
    const uint8_t *bytes, uint32_t nb_bytes) {

    return 0;
}

void handle_tag(struct slipstub *s, uint16_t tag, const struct pbuf *p) {
    //infof("tag %d\n", tag);
    switch(tag) {
    case TAG_U32: {
        /* name ! {send_u32, [101, 1000000000, 1,2,3]}. */
        int rv = tag_u32_dispatch(handle_tag_u32, NULL, p->buf, p->count);
        if (rv) { infof("tag_u32_dispatch returned %d\n", rv); }
        break;
    }
    default:
        infof("unknown tag 0x%x\n", tag);
    }
}


/* STARTUP */


void start(void) {
    hw_app_init();
    CBUF_INIT(hw_event);

    rcc_periph_clock_enable(RCC_GPIOA | RCC_GPIOB | RCC_AFIO);

    /* Use framwork for handling incoming USB SLIP commands. */
    slipstub_init(handle_tag);

    hw_delay_init(C_TIM, 0xFFFF, 1 /*enable interrupt*/);
    hw_delay_arm(C_TIM, 1000);
    hw_delay_trigger(C_TIM);

    _service.add(poll_event);


    swtimer_schedule(&swtimer, 0x1000,0);
    swtimer_schedule(&swtimer, 0x2000,0);
    swtimer_schedule(&swtimer, 0x3000,0);


    infof("cspisr\n");

}
void stop(void) {
    hw_app_stop();
    _service.reset();
}

#ifndef VERSION
#define VERSION "current"
#endif

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "CSP ISR TEST";
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;
const char config_version[]      CONFIG_DATA_SECTION = VERSION;
const char config_protocol[]     CONFIG_DATA_SECTION = "slip";

struct gdbstub_config config CONFIG_HEADER_SECTION = {
    .manufacturer    = config_manufacturer,
    .product         = config_product,
    .firmware        = config_firmware,
    .version         = config_version,
    .protocol        = config_protocol,
    .start           = start,
    .stop            = stop,
    .switch_protocol = slipstub_switch_protocol,
};

