#ifndef MOD_TIMEBASE_C
#define MOD_TIMEBASE_C

/* A single general-purpose timer is used to provide two time-based
   mechanisms at different time scales:

   - The ability to measure microsecond-level time differences for
     decoding pulse width modulation.

   - A periodic interrupt to schedule millisecond-level events for
     output pulse width and overall timeout.

   Time differences are simplest to perform when the timer runs over
   the entire 16 bit range.  That requires div=0x10000 and gives a
   base clock of

   (/ 72000.0 #x10000) 1.1 kHz or 0.9us

   This fits perfectly tot the millisecond-level requirement for the
   interrupt, so set the prescaler to 1.
*/

#ifndef TIMEBASE_DIV
#error  TIMEBASE_DIV
#endif

const struct hw_periodic hw_tim_10us[] = {
//          rcc       irq            tim   div           pre
//-----------------------------------------------------------
    [2] = { RCC_TIM2, NVIC_TIM2_IRQ, TIM2, TIMEBASE_DIV, 1 },
    [3] = { RCC_TIM3, NVIC_TIM3_IRQ, TIM3, TIMEBASE_DIV, 1 },
};

// TIM 2 interferes with flashing
#define TIMEBASE_TIM 3
#define TIMEBASE_C_TIM hw_tim_10us[TIMEBASE_TIM]


/* Number of integral ticks per millisecond. */
#define ITICKS_PER_MS (72000.0/((double)TIMEBASE_DIV))

/* Number of microseconds per fractional tick. */
#define US_PER_FTICK ((double)(1.0/72.0))


/* The integral number of 1.1kHz ticks is used for alarm events. */
volatile uint32_t timebase_ticks;
#ifdef TIMEBASE_ALARM
/* The current hardware timer value is used for fine scale
 * measurements. */
volatile uint16_t timebase_frac_mark;

static inline void timebase_set_mark(void) {
    timebase_frac_mark = hw_tim_counter(TIMEBASE_C_TIM.tim);
}

/* This relies on unsigned rollover, so only works on full scale timer. */
#if TIMEBASE_DIV == 0x10000
static inline uint32_t timebase_elapsed_us(void) {
    uint16_t fticks = hw_tim_counter(TIMEBASE_C_TIM.tim) - timebase_frac_mark;
    uint32_t us = (((double)fticks) * US_PER_FTICK);
    // infof("us = %d\n", us);
    return us;
}
#endif

volatile uint32_t timebase_alarm_mark;
volatile uint32_t timebase_alarm_enable;
#endif
void HW_TIM_ISR(TIMEBASE_TIM)(void) {
    hw_periodic_ack(TIMEBASE_C_TIM);
    uint32_t ticks = timebase_ticks++;
#ifdef TIMEBASE_ALARM
    if (timebase_alarm_enable && (timebase_alarm_mark == ticks)) {
        timebase_alarm_enable = 0;
        TIMEBASE_ALARM();
    }
#endif
}
static inline void timebase_alarm_start_ms(uint32_t ms) {
    uint32_t tticks = (uint32_t)(ITICKS_PER_MS*((double)ms));
    //infof("alarm_start: ms=%d, tticks=%d\n", ms, tticks);
    timebase_alarm_enable = 1;
    timebase_alarm_mark = timebase_ticks + tticks;
}

static inline void timebase_init(void) {
    hw_periodic_init(TIMEBASE_C_TIM);
}


#endif
