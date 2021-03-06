/* See also notes in cspisr.c

   This is another test for a 3-level application structure.

   1) TOKEN: Timer ISR, Fast hardware events.  Runs state machines
      that only do fast dispatch, but e.g. no data processing (no
      loops).

   2) PACKET: Software ISR at lower priority, run state machines that
      perform loops e.g. packet processing.

   3) APP: Polling application WFI / WFE main loop, anything that is
      not real-time.

   Communication within one priority level can be implemented on top
   of shared memory.

   Hi->Lo communication is software interrupt for 1->2 and queues and
   counters for 1->3 and 2->3.

   Communication 3->1 and 3->2 can be SWI.


   Think about this as a combination of 2 ways to look at tasks:

   - Each level is an asynchronous actor, and can only communicate
     with other actors through message passing.

   - Inside each actor, a synchronous world can execute, so any
     cooperative task composition mechanism can be used.

   The big idea here is to combine synchronous and asynchronous
   message passing in a natural way.


*/

#ifndef MOD_3LEVEL
#define MOD_3LEVEL

#include "cbuf.h"
#include "swtimer.h"

#define TICKS_PER_US 100

const struct hw_delay hw_tim[] = {
//          rcc       irq            tim   psc
//-------------------------------------------------------
    [3] = { RCC_TIM3, NVIC_TIM3_IRQ, TIM3, 72*TICKS_PER_US },
};
#define TIM 3
#define C_TIM hw_tim[TIM]

struct cbuf mbox_app;
uint8_t     mbox_app_buf[16];

swtimer_element_t timer_elements[10]; // FIXME: how many?
struct swtimer timer = { .nb = 0, .arr = timer_elements };


/* 1. TOKEN LEVEL */

/* 1.a Synchronous update method. */
void token_tick(swtimer_element_t e) {
    /* Here we can communicate with the app... */
    cbuf_put(&mbox_app, e.tag);
    /* ... and schedule new timeouts. */
    static int again;
    if (again++ < 10) {
        swtimer_schedule(&timer, 0x1000, e.tag+10);
    }

}

/* 1.b Timer interrupt. */

/* _ack disables counter, _arm sets period and counter register, then
   _trigger enables counter.  Will the timer keep updating on the same
   prescaler grid?  I would like to keep to keep it on a perfect grid
   so handle times do not cause jitter..

   FIXME: It seems that a better approach is to let the counter
   auto-reset, but only update the period register?
*/

void HW_TIM_ISR(TIM)(void) {
    hw_delay_ack(C_TIM);
    swtimer_element_t e;

  pop:
    /* Top of the heap is the event we're currently handling. */
    ASSERT(swtimer_nb(&timer));
    e = swtimer_pop(&timer);

    /* Reset the logical time base before passing it to token_tick(),
       because it might insert new relative timeout events. */
    swtimer_shift(&timer, e.time_abs);
    token_tick(e);

    /* If the next event is at the same logical instance, handle it.
       Otherwise set the hardware timer to resume. */
    if (swtimer_nb(&timer)) {
        uint16_t diff = swtimer_diff(&timer);
        if (!diff) goto pop;
        hw_delay_arm(C_TIM, diff);
        hw_delay_trigger(C_TIM);
    }
}
/* 1.b Software interrupt. */
/* Cancel current timer event, run token_tick() and reschedule the timer. */


/* 2. PACKET LEVEL */
/* Software interrupt */


/* 3. APP LEVEL */
/* Poll routine */
void poll_mbox_app(void) {
    while(cbuf_bytes(&mbox_app)) {
        infof("tag = %d\n", cbuf_get(&mbox_app));
    }
}

void init_3level(void) {

    CBUF_INIT(mbox_app);

    /* FIXME: swtimer can only be accessed in the isr.
       Setting timer events probably needs a separate mechanism. */

    for (int i=1; i<=3; i++) {
        swtimer_schedule(&timer,0x1000*i,i);
    }

    hw_delay_init(C_TIM, 0xFFFF, 1 /*enable interrupt*/);
    if (swtimer_nb(&timer)) {
        /* +1 because we can't set a delay of 0. */
        hw_delay_arm(C_TIM, 1 + swtimer_diff(&timer));
        hw_delay_trigger(C_TIM);
    }

    _service.add(poll_mbox_app);

}

#endif
