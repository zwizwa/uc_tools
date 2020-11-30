// Requires:
// - HW_EVENT_TIMEOUT
// - struct cbuf hw_event
//
// Optional:
// - STAT_INC


/* Timeout interrupt. */

/* FIXME: It is not trivial to keep timer grid-aligned on the STM
   together with variable timeouts, so we don't bother.  The next
   timeout is set in the bottom half event handler, so there is jitter
   due to the response time.  If grid-aligned time is needed, use a
   periodic timer instead.  ( Reloading a periodic timer is possible
   but introduces a possible race condition for short timeouts. ) */

/* ISR only moves token into the hw_event queue.  All software timer
   manipulations are handled there, so no locks are needed on the data
   structure. */
void HW_TIM_ISR(TIM)(void) {
#ifdef STAT_INC
    STAT_INC(tim_isr_count);
#endif
    cbuf_put(&hw_event, HW_EVENT_TIMEOUT);
    hw_delay_ack(C_TIM);
}
swtimer_element_t timer_elements[10]; // FIXME: how many?
swtimer_element_t timer_last;
struct swtimer timer = { .nb = 0, .arr = timer_elements };

/* There are two access points into the timer:
   - user: set delay with tag
   - system: handle current timeout, set the next one
*/

/* It is hard to express as a proper transaction!

   The problem is that there is a race condition between setting a new
   timeout in a low priority bottom half, and handling the old one in
   the main isr.  I'm thinking it would be much simpler to use a
   system tick instead.

*/


