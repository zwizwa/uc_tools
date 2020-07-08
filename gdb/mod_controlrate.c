#ifndef MOD_CONTROLRATE_C
#define MOD_CONTROLRATE_C

/* CONTROL RATE TIMER INTERRUPT */

/* We are called from sample clock source, e.g. PWM timer at a
   subdivided rate, and then will further subdivide to run a "beat"
   task in the main poll loop. */

const struct hw_swi hw_control = HW_SWI_1;
#define C_CONTROL     hw_control
#define C_CONTROL_ISR exti1_isr


#define CONTROLRATE_BEAT_DIV 1024

struct controlrate {
    uint32_t swi_count;
    uint32_t isr_count;
    uint32_t beat_pulse;
    uint32_t beat_handled;
};
volatile struct controlrate controlrate;


/* Interrupt context so it can pre-empt the main loop. */
static inline void control_update(void) {
    /* Signal synchronous main loop task. */
    if ((controlrate.isr_count % CONTROLRATE_BEAT_DIV) == 0) {
        controlrate.beat_pulse++;
    }
    controlrate.isr_count++;
}
void C_CONTROL_ISR(void) {
    hw_swi_ack(C_CONTROL);
    control_update();
}




/* Main loop context. */
void controlrate_beat_poll(void) {
    if (controlrate.beat_handled != controlrate.beat_pulse) {
        controlrate.beat_handled++;
        //infof("log_period = %d\n", log_period);
    }
}


static inline void control_trigger(void) {
    hw_swi_trigger(C_CONTROL);
    controlrate.swi_count++;
}
void controlrate_init(gdbstub_fn_add service_add) {
    hw_swi_init(C_CONTROL);
    service_add(controlrate_beat_poll);
}



#endif
