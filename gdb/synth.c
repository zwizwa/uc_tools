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

#include <stdint.h>
#include "fixedpoint.h"
#include "base.h"

#include "gdbstub_api.h"
#include <string.h>


/* CONFIGURATION */

#define FOR_PARAMETERS(p) \
    p(0, uint32, osc_setpoint, (15 << 27), "Main oscillator setpoint, in 5.27 nlog2(period_cycles)") \
    p(1, uint32, osc_glide,    0x00100000, "Main oscillator regulator gain, 0.32 fixed point, units: measurement/control") \
    p(2, uint32, osc_mfilter,  0x01000000, "Main oscillator measurement anit-aliasing filter pole 0.32 fixed point") \

union types {
    uint32_t uint32;
};
#define DEF_PARAMETER(_num, _type, _name, _init, _info) \
    volatile union types _name = { ._type = _init };
FOR_PARAMETERS(DEF_PARAMETER)

struct parameter_info {
    const char *type;
    const char *name;
    const char *info;
    union types init;
    volatile union types *data;
    uint32_t size;
};
struct parameter_info parameter_info[] = {
#define DEF_PARAMETER_INFO(_num, _type, _name, _init, _info)            \
    [_num] = {                                                          \
        .type = #_type,                                                 \
        .name = #_name,                                                 \
        .info = _info,                                                  \
        .init._type = _init,                                            \
        .data = &_name,                                                 \
        .size = sizeof(_name)                                           \
    },
FOR_PARAMETERS(DEF_PARAMETER_INFO)
};



#include "mod_pdm.c"


#include "fixedpoint_log.h"
uint32_t feynman_table[] = { FEYNMAN_TABLE_INIT };
static inline uint32_t nlog2(uint32_t arg) {
    return feynman_nlog_5_27(feynman_table, FEYNMAN_PRECISION, arg);
}
#if 0
struct fixedpoint_svf filter = {
    .ai = 0xFF000000ULL,
    .ar = 0x01000000ULL,
    .c0 = 0xFFFFFFFFULL,
    .c1 = 0xFFFFFFFFULL,
    .c2 = 0xFFFFFFFFULL,
};
KEEP int32_t ortho_test(int32_t input) {
    return fixedpoint_svf_update(&filter, input);
}
#endif






/* Protocol-wise we don't really need anything special, so use
   slipstub_buffered to do the basics (SLIP framing, PING, GDB, INFO)
   and use tag_u32 for app-specific commands. */
#include "tag_u32.h"
#include "slipstub.h"
struct slipstub slipstub;
struct slipstub_buffers slipstub_buffers;



/* OSCILLATOR PULSE INTERRUPT */

const struct hw_exti c_exti = {
//  rcc_gpio   nvic_irq            pin  gpio   trigger
    RCC_GPIOA, NVIC_EXTI0_IRQ,      0,  GPIOA, EXTI_TRIGGER_FALLING
};

#define C_OSC c_exti
#define EXTI_GPIO GPIOA,0
volatile uint32_t osc_period, nb_pulses, last_cc;

#include "cycle_counter.h"

#define SUBOSC_GPIOB_PIN 1
#define SUBOSC_GPIO GPIOB,SUBOSC_GPIOB_PIN
static inline void init_subosc(void) {
    hw_gpio_config(SUBOSC_GPIO, HW_GPIO_CONFIG_OUTPUT);
}

void exti0_isr(void) {

    /* CRITICAL CONSTANT LATENCY */

    /* Note that the Schmitt trigger discharge pulse derived from the
       buffered SAW wave is stressing that 74HC11 and has a slow rise
       time.  I've double buffered it through the Schmitt trigger,
       which straightens the edges before they go into the STM.
       Otherwise there was odd EXTI behavior. */

    hw_exti_ack(C_OSC);

    /* Reset the PWM digital state machine. */
    pwm_phase = 0;

    /* Update the sub osc. */
    GPIOB_ODR ^= (1 << SUBOSC_GPIOB_PIN);

    /* Time base is ARM cycle counter @72MHz.  The instruction stream
       above has constant latency, so it's ok to sample here. */
    uint32_t cc = cycle_counter();



    /* NON CRITICAL LATENCY */

#if 0
    /* Do not do any filtering here.  Write the current measurement,
       and do other computations in the control loop. */

 // FIXME: probably this is not necessary.
    osc_period = cc - last_cc;
#else
    /* New measurement is the elapsed time since last capture. */
    int32_t measurement = cc - last_cc;

    /* We filter that using a first order filter.  Note that this is
       updated at the oscillator's current rate, so the filter time
       constant in real time depends on the frequency of the
       oscillator. */
    int32_t estimate = osc_period;
    int32_t coef = (osc_mfilter.uint32 >> 1);
    int32_t diff = measurement - estimate;
    int32_t update = (I64(coef) * I64(diff)) >> 31;
    estimate += update;

    /* Save state */
    osc_period = estimate;
#endif

    last_cc = cc;
    nb_pulses++;
}

void init_osc_in(void) {
    /* External input interrupt for discharge pulse.  This needs to be
       lower priority than the PDM because it will cause modulation
       effects. */
    enable_cycle_counter();
    hw_exti_init(C_OSC);
    hw_exti_arm(C_OSC);
}



/* CONTROL RATE TIMER INTERRUPT */

#define BEAT_DIV 1024

volatile uint32_t control_count;

volatile uint32_t beat_pulse;
volatile uint32_t beat_handled;
volatile uint32_t log_period;

/* Interrupt context so it can pre-empt the main loop. */
void control_update(void) {
    /* 5.27 negative fixed point base 2 log. */

    // FIXME: 1. double buffer the pdm setpoint, 2. do linear
    // interpolation of the actual set point.

    /* We regulate the log_period to the setpoint using a linear
       integrating regulator.

       The basic first order integrating control algorithm is:

          c += g e
          e  = s - m

       Where:

          c   control input        (channel[0].setpoint)
          g   loop gain            (osc_glide)
          e   error signal         (error)
          s   setpoint             (osc_setpoint)
          m   output measurement   (osc_measurement)


       The measurement and setpoint ar in 5.27 negative fixed point
       base 2 log.  The log operation ensures that the relationship
       between the control and measurement is roughly linear.  Any
       remaining non-linearity can be handled by the feedback.

       There are a couple of inversions in the control and feedback
       chain:

       - setpoint: inc
       - 74HC14 inverting buffer: dec
       - saw frequency: dec
       - saw period: inc
       - perod nlog2: dec

       So from setpoint to nlog2 we have an inversion.  Expressing the
       algorithm in standard form where the measurement has a minus
       sign, we need to add another minus sign in the loop to
       compensate for the extra inversion, e.g. "-="

       The control pole is in 0.32 form, but we perform 1.31 signed
       arithmetic because the error is signed, so it is shifted by
       one.
    */

    uint32_t osc_measurement = nlog2(osc_period);
    int32_t e = osc_setpoint.uint32 - osc_measurement;
    int32_t g = osc_glide.uint32 >> 1;
    int32_t g_e = (I64(g) * I64(e)) >> 31;
    channel[0].setpoint -= g_e;

    /* Signal synchronous main loop task. */
    if ((control_count % BEAT_DIV) == 0) {
        beat_pulse++;
    }
    control_count++;
}

/* Main loop context. */
void beat_poll(void) {
    if (beat_handled != beat_pulse) {
        beat_handled++;
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
void init_control(void) {
    hw_swi_init(C_CONTROL);
    hw_swi_arm(C_CONTROL);
}



/* COMMUNICATION */


void set_parameter(uint32_t index, void *buf, uint32_t len) {
    if (index >= ARRAY_SIZE(parameter_info)) {
        infof("%d: bad_param\n", index);
        return;
    }
    struct parameter_info *pi = &parameter_info[index];
    if (pi->size != len) {
        infof("%d: bad_size: %d\n", index, len);
        return;
    }
    /* FIXME: signed ints not yet supported. */
    uint32_t val = read_be(buf, len);
    pi->data->uint32 = val;
    infof("%d: %s = %d\n", index, pi->name, val);
}



/* Ad-hoc multi-argument messages are not in the table. */
int handle_tag_u32(void *context,
                   const uint32_t *arg,  uint32_t nb_args,
                   const uint8_t *bytes, uint32_t nb_bytes) {
    if (nb_args < 1) return -1;
    switch(arg[0]) {
        /* Ad hoc */
    case 101: { // TEST_UPDATE
        // bp2 ! {send_u32, [101, 1000000000, 1,2,3]}.
        if (nb_args > 1 + NB_CHANNELS) return -1;
        for (int i=0; i<nb_args-1; i++) {
            channel[i].setpoint = safe_setpoint(arg[1+i]);
        }
        uint32_t shiftreg_gpio = channels_update();
        for (int i=0; i<nb_args-1; i++) {
            infof("%d: %x %x\n", i, channel[i].accu, channel[i].setpoint);
        }
        infof("gpio %x\n", shiftreg_gpio);
        return 0;
    }
    case 102: { // TEST_INFO
        infof("bsrr_last = %x\n", bsrr_last);
        for (int i=0; i<NB_CHANNELS; i++) {
            infof("%d: %x %x\n", i, channel[i].accu, channel[i].setpoint);
        }
        infof("osc_period: %d\n", osc_period);
        infof("nb_pulses:  %d\n", nb_pulses);
        infof("last_cc:    %d\n", last_cc);
        return 0;
    }
    case 1: // MODE
        //  bp2 ! {send_packet, <<16#FFF50002:32, 1:32, 1:32}.
        if (nb_args < 2) return -1;
        if (arg[1]) { pdm_start(); }
        else        { pdm_stop(); }
        return 0;
    case 2: { // SETPOINT
        struct { uint32_t cmd; uint32_t chan; uint32_t val; } *a = (void*)arg;
        if (nb_args < 3) return -1;
        if (a->chan >= NB_CHANNELS) return -2;
        channel[a->chan].setpoint = safe_setpoint(a->val);
        infof("setpoint[%d] = %x\n", a->chan, channel[a->chan].setpoint);
        return 0;
    }

    default:
        infof("unknown tag_u32 command:");
        for (int i=0; i<nb_args; i++) { infof(" %d", arg[i]); }
        infof("\n");
        return -2;
    }
}

/* slipstub calls this one for application tags.  We then patch
   through to command handler. */
void handle_tag(struct slipstub *s, uint16_t tag, const struct pbuf *p) {
    infof("tag %d\n", tag);
    switch(tag) {
    case 0: {
        if (p->count < 4) {
            infof("short packet\n");
        }
        else {
            uint32_t index = read_be(p->buf+2, 2);
            set_parameter(index, p->buf+4, p->count-4);
        }
        break;
    }
    case TAG_U32: {
        int rv = tag_u32_dispatch(handle_tag_u32, NULL, p->buf, p->count);
        if (rv) {
            infof("tag_u32_dispatch returned %d\n", rv);
        }
        break;
    }
    default:
        infof("unknown tag 0x%x\n", tag);
    }
}




/* STARTUP */
void start(void) {
    hw_app_init();
    /* FIXME: This assumes it's GPIOA */
    rcc_periph_clock_enable(RCC_GPIOA | RCC_GPIOB | RCC_AFIO);

    /* Use framwork for handling incoming USB SLIP commands. */
    slipstub_init(handle_tag);

    /* PDM CV outputs */
    pdm_init();
    pdm_start();

    /* Main oscillator frequency feedback. */
    init_osc_in();

    /* Suboscillator */
    init_subosc();

    /* Turn off the LED.  It introduces too much noise. */
    hw_gpio_config(GPIOC,13,HW_GPIO_CONFIG_INPUT);

    /* Main loop tasks. */
    _service.add(beat_poll);

    /* Smaller values mean higher priorities.  STM32F103 strips the
       low 4 bits, so these have increments of 16.  Print the actual
       value at boot to be sure. */
    NVIC_IPR(C_PDM.irq)     = 0;
    NVIC_IPR(C_OSC.irq)     = 16;
    NVIC_IPR(C_CONTROL.irq) = 32;
    infof("pri: pdm     %d\n", NVIC_IPR(C_PDM.irq));
    infof("pri: osc     %d\n", NVIC_IPR(C_OSC.irq));
    infof("pri: control %d\n", NVIC_IPR(C_CONTROL.irq));

    //infof("C_CONTROL.div = %d\n", C_CONTROL.div);

}
void stop(void) {
    hw_app_stop();
    _service.reset();
}
const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "Pulse Density Modulator";
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;
const char config_version[]      CONFIG_DATA_SECTION = BUILD;
const char config_protocol[]     CONFIG_DATA_SECTION = "{driver,pdm,slip}";

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

