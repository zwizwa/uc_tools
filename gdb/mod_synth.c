#ifndef MOD_SYNTH_C
#define MOD_SYNTH_C

/* Digitally controlled hybrid analog synth.
   - PDM CV generators.
   - Analog sawtooth.
   - Digital subosc
   - Digital hard-sync 8-bit wavetable (PWM)
   - Digitial feedback control

   TODO:
   - Start mapping parameters to midi
   - Set frequency, glide
   - Add some dynamic wavetables
   - Use EXTI software events for control updates
*/


#include "tag_u32.h"
#include "parameter.h"


/* Toplevel compile-time parameterizable module containing a synth
   engine + communication protocol. */

/* Some alternative time bases.  Quantizer needs a power of two
 * divisor. */

#define PDM_DIV_LOG 8
#define PDM_DIV (1 << PDM_DIV_LOG)
#define PWM_HZ (72000000 / PDM_DIV)



/* Core routines are modularized so they can be macro-parameterized
   across several firmware images. */
//#include "mod_pdm.c"           // PWM/PDM
#include "mod_pdm_pwm.c"       // Multi-PWM version, mostly compatible
#include "mod_osc.c"           // Sync in, subosc, pmeas
#include "mod_controlrate.c"   // Control rate + Beat rate timescales

/* This defines the "atomic" parameters that are picked up by the
   synth engine at the next cycle.  For event-like paremeters starting
   at offset 100, see the handler function. */

/* Init is at the end here to allow non-scalar parameters that would
   contain an initializer.  Ideally, code at the other end will use
   symbolic names, and not parameter ids. */

#define SYNTH_FOR_PARAMETERS(p) \
    p(0, uint32,   osc_setpoint, "Main oscillator setpoint, in 5.27 nlog2 (period_cycles)", (15 << 27)) \

SYNTH_FOR_PARAMETERS(DEF_PARAMETER)
struct parameter_info parameter_info[] = {
SYNTH_FOR_PARAMETERS(DEF_PARAMETER_INFO)
};



void synth_init(void) {

    /* PDM CV outputs */
    pdm_init();
    pdm_start();

    /* Main oscillator frequency feedback + subosc. */
    osc_init(_service.add, slipstub.slip_out);

    /* Control rate task. */
    controlrate_init(_service.add);

    /* Smaller values mean higher priorities.  STM32F103 strips the
       low 4 bits, so these have increments of 16.  Print the actual
       value at boot to be sure. */
    NVIC_IPR(C_PDM.irq)     = 0;
    NVIC_IPR(C_OSC.irq)     = 16;
    NVIC_IPR(C_CONTROL.irq) = 32;
    infof("pri: pdm     %d\n", NVIC_IPR(C_PDM.irq));
    infof("pri: osc     %d\n", NVIC_IPR(C_OSC.irq));
    infof("pri: control %d\n", NVIC_IPR(C_CONTROL.irq));
}

/* Ad-hoc multi-argument messages are not in the table. */
int synth_handle_tag_u32(struct tag_u32 *s) {

    if (s->nb_args < 1) return -1;
    switch(s->args[0]) {

        /* Ad hoc commands that do not fit the simple parameter table
           structure. */

    case 100: { // MODE
        //  bp2 ! {send_packet, <<16#FFF50002:32, 100:32, 1:32}.
        if (s->nb_args < 2) return -1;
        if (s->args[1]) { pdm_start(); }
        else            { pdm_stop(); }
        return 0;
    }
    case 101: { // SETPOINT
        struct { uint32_t cmd; uint32_t chan; uint32_t val; } *a = (void*)s->args;
        if (s->nb_args < 3) return -1;
        if (a->chan >= PDM_NB_CHANNELS) return -2;
        pdm_channel[a->chan].setpoint = pdm_safe_setpoint(a->val);
        // infof("setpoint[%d] = %x\n", a->chan, channel[a->chan].setpoint);
        return 0;
    }
    case 102: { // MEASURE
        if (s->nb_args >  2) { return -3; }
        if (s->nb_args == 2) {
            pmeas_state.log_max = s->args[1];
            // infof("pmeas_state.log_max = %d\n", pmeas_state.log_max);
        }
        if (s->nb_bytes) {
            // Binary payload is the continuation.  The following
            // measurement result will be forwarded.
            //infof("measurement_wait queue: %d\n", nb_bytes);
            struct cbuf *b = &measurement_wait;
            cbuf_put(b, s->nb_bytes);
            cbuf_write(b, s->bytes, s->nb_bytes);
        }
        return 0;
    }

    default:
        // Atomic parameters.
        // parameter.h supports the tag_u32 protocol
        return tag_u32_set_parameter(
            &parameter_info[0], ARRAY_SIZE(parameter_info),
            s->args, s->nb_args,
            s->bytes, s->nb_bytes);
    }
}


#endif
