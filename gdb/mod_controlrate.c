#ifndef MOD_CONTROLRATE_C
#define MOD_CONTROLRATE_C

/* CONTROL RATE TIMER INTERRUPT */

/* We are called from sample clock source, e.g. PWM timer at a
   subdivided rate, and then will further subdivide to run a "beat"
   task in the main poll loop. */

#define CONTROLRATE_BEAT_DIV 1024

struct controlrate {
    uint32_t count;
    uint32_t beat_pulse;
    uint32_t beat_handled;
};
volatile struct controlrate controlrate;


/* Interrupt context so it can pre-empt the main loop. */
void control_update(void) {
    /* Signal synchronous main loop task. */
    if ((controlrate.count % CONTROLRATE_BEAT_DIV) == 0) {
        controlrate.beat_pulse++;
    }
    controlrate.count++;
}

/* Main loop context. */
void controlrate_beat_poll(void) {
    if (controlrate.beat_handled != controlrate.beat_pulse) {
        controlrate.beat_handled++;
        //infof("log_period = %d\n", log_period);
    }
}

/* Use a software interrupt triggered as a subdiv from pwm/pdm interrupt. */
const struct hw_swi hw_control = HW_SWI_1;
#define C_CONTROL hw_control

static inline void control_trigger(void) {
    hw_swi_trigger(C_CONTROL);
    control_update();
}
void controlrate_init(gdbstub_fn_add service_add) {
    hw_swi_init(C_CONTROL);
    hw_swi_arm(C_CONTROL);
    service_add(controlrate_beat_poll);
}



#endif
