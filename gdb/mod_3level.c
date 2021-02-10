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

/* 1.a Syncrhonous update method. */
void token_tick(swtimer_element_t now) {
    /* Here we can communicate with the app... */
    cbuf_put(&mbox_app, now.tag);
    /* ... and schedule new timeouts. */
    //swtimer_schedule(&timer, 0x1000, now.tag+10);

}

/* 1.b Timer interrupt. */

/* FIXME: How to ensure timing stays on a perfect grid?  We should
   probably use the actual time instead of the logical time.  Or is it
   already on a perfect grid?  Read datasheet again... */

void HW_TIM_ISR(TIM)(void) {
    /* Perform bookkeeping first: acknowledge interrupt, save the
       current event for processing, and advance the software
       timer. */
    hw_delay_ack(C_TIM);

    /* Pop the timer.  There is guaranteed to be a single entry here,
       since the only way we can trigger an interrupt is from
       inspecting a timer heap with at least one element. */
    swtimer_element_t now;
    for (;;) {
        now = swtimer_pop(&timer);

        /* Reset the time base.  This is necessary because token_tick()
           might insert new events.  */
        swtimer_shift(&timer, now.time_abs);

        /* Then pass the event to the token processor. */
        token_tick(now);

        /* If the next event is not now, set the hardware timer,
           otherwise keep handling. */
        if (swtimer_nb(&timer)) {
            swtimer_element_t next = swtimer_peek(&timer);
            uint16_t diff = next.time_abs - timer.now_abs;
            if (diff) {
                hw_delay_arm(C_TIM, diff);
                hw_delay_trigger(C_TIM);
                return;
            }
        }
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
    hw_delay_arm(C_TIM, 1); /* timeout needs to be >0 */
    hw_delay_trigger(C_TIM);

    _service.add(poll_mbox_app);

}

#endif
