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


   TODO:
   - Make sure the swtimer works properly
   - Create two ISR levels: hw events, sw queue handler
   - Set up a transmit, reply/timeout program
   - Generalize to 3xUART RX/TX, 3xGPIO, timer all from ISR
   - Can STM uart do start bit detection interrupt?

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



/* Timer.
   Operation is split into two parts:
   - ISR pushes event into queue
   - Handler which resets the timer, and also handles timer events.
*/

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


#define STAT_INC(var) stat.var++
#include "mod_swtimer.c"


static void poll_event(void) {
    uint16_t token;
    while (CBUF_EAGAIN != (token = cbuf_get(&hw_event))) {

        switch(token) {
        case HW_EVENT_TIMEOUT: {
            infof("timeout %x %d\n", timer_last.time_abs, timer_last.tag);
            swtimer_element_t next;
            if (swtimer_next(&timer, &next)) {
                uint16_t diff = next.time_abs - timer_last.time_abs;
                timer_last = next;
                hw_delay_arm(C_TIM, diff);
                hw_delay_trigger(C_TIM);
            }
            break;
        }
        default:
            //infof("event %d\n", token);
            //if (!csp_send(&sched, CHAN_HW_EVENT, &token, sizeof(token))) {
            //    infof("CSP hw_event dropped\n");
            //}
            break;
        }
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

int handle_tag_u32(struct tag_u32 *s) {
    return -1;
}

void handle_tag(struct slipstub *s, uint16_t tag, const struct pbuf *p) {
    //infof("tag %d\n", tag);
    switch(tag) {
    case TAG_U32: {
        /* name ! {send_u32, [101, 1000000000, 1,2,3]}. */
        int rv = tag_u32_dispatch(
            handle_tag_u32,
            NULL, //send_reply_tag_u32,
            NULL, p->buf, p->count);
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

    /* FIXME: swtimer can only be accessed in the isr.
       Setting timer events probably needs a separate mechanism. */

    for (int i=1; i<=3; i++) {
        swtimer_schedule(&timer,0x1000*i,i);
    }

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

