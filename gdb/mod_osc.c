#ifndef MOD_OSC_C
#define MOD_OSC_C

/* CONFIGURATION

   The ISR is configured explicitly here: it is not implemented in a
   regular way on STM32F103: some are associated to multiple pins, so
   configure it separately. */
#define C_OSC     c_exti[0]
#define C_OSC_ISR exti0_isr



#include "cbuf.h"
#include "uct_byteswap.h"

/* Ad hoc module for oscillator pulse input, frequency measurement and
 * sub osc output. */

/* OSCILLATOR PULSE INTERRUPT */

const struct hw_exti c_exti[] = {
//          rcc_gpio   nvic_irq         pin  gpio   trigger
    [0] = { RCC_GPIOA, NVIC_EXTI0_IRQ,   0,  GPIOA, EXTI_TRIGGER_FALLING },
};


#include "cycle_counter.h"

#define OSC_SUB_GPIOB_PIN 10
#define OSC_SUB_GPIO GPIOB,OSC_SUB_GPIOB_PIN
static inline void osc_sub_init(void) {
    hw_gpio_config(OSC_SUB_GPIO, HW_GPIO_CONFIG_OUTPUT);
}

#include "pmeas.h"

volatile struct pmeas_state pmeas_state = {};

/* This buffer stores continuations for reply messages.  E.g. pids of
   waiting erlang tasks.  An Erlang pid is 27 bytes, plus one byte for
   the size.  So this can fit two pids. */
struct cbuf measurement_wait; uint8_t measurement_wait_buf[64]; // FIXME



void C_OSC_ISR(void) {

    /* CRITICAL CONSTANT LATENCY */

    /* Note that the Schmitt trigger discharge pulse derived from the
       buffered SAW wave is stressing that 74HC11 and has a slow rise
       time.  I've double buffered it through the Schmitt trigger,
       which straightens the edges before they go into the STM.
       Otherwise there was odd EXTI behavior. */

    hw_exti_ack(C_OSC);


#ifdef OSC_HARD_SYNC
    OSC_HARD_SYNC();
#endif

    /* Update the sub osc. */
    GPIOB_ODR ^= (1 << OSC_SUB_GPIOB_PIN);

    /* Time base is ARM cycle counter @72MHz.  The instruction stream
       above has constant latency, so it's ok to sample here. */
    uint32_t cc = cycle_counter();


    /* NON CRITICAL LATENCY */
    pmeas_update(&pmeas_state, cc);
}


void osc_pmeas_state_poll(void) {
    pmeas_state_poll_write(&pmeas_state);
}


void osc_init(gdbstub_fn_add service_add, struct cbuf *b_reply) {
    /* External input interrupt for discharge pulse.  This needs to be
       lower priority than the PDM because it will cause modulation
       effects. */
    CBUF_INIT(measurement_wait);

    /* One second is about 1<26 cycles. */
    pmeas_state.log_max = 26;
    pmeas_state.b_reply = b_reply;
    pmeas_state.b_wait = &measurement_wait;

    enable_cycle_counter();
    hw_exti_init(C_OSC);
    hw_exti_arm(C_OSC);

    /* Suboscillator */
    osc_sub_init();

    /* Register polling routine. */
    service_add(osc_pmeas_state_poll);

}




/* FIXME: Currently log is used in calibration routine on PC.  This
   will have to be moved here eventually. */
#include "fixedpoint_log.h"
uint32_t feynman_table[] = { FEYNMAN_TABLE_INIT };
static inline uint32_t nlog2(uint32_t arg) {
    return feynman_nlog_5_27(feynman_table, FEYNMAN_PRECISION, arg);
}


#endif
