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
*/

#include <stdint.h>
#include "fixedpoint.h"
#include "base.h"

#include "gdbstub_api.h"
#include <string.h>


#include "mod_pdm.c"



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

#define C_EXTI c_exti
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

    hw_exti_ack(C_EXTI);

    /* Reset the PWM digital state machine. */
    pwm_phase = 0;

    /* Update the sub osc. */
    GPIOB_ODR ^= (1 << SUBOSC_GPIOB_PIN);

    /* Time base is ARM cycle counter @72MHz.  The instruction stream
       above has constant latency, so it's ok to sample here. */
    uint32_t cc = cycle_counter();



    /* NON CRITICAL LATENCY */

    /* New measurement is the elapsed time since last capture. */
    uint32_t osc_period_new = cc - last_cc;

    /* We filter that using a first order filter.  Note that this is
       updated at the oscilattor's current rate, so the filter pole
       depends on the frequency of the oscillator. */
    uint32_t osc_period_estimate = osc_period;
    uint32_t coef = 0xFF000000ULL;
    osc_period_estimate =
        fixedpoint_mul(coef,  osc_period_estimate) +
        fixedpoint_mul(~coef, osc_period_new);
    // FIXME: create a signed multiplication

    /* Save state */
    osc_period = osc_period_estimate;
    last_cc = cc;
    nb_pulses++;
}

void init_osc_in(void) {
    /* External input interrupt for discharge pulse.  This needs to be
       lower priority than the PDM because it will cause modulation
       effects. */
    enable_cycle_counter();
    hw_exti_init(C_EXTI);
    hw_exti_arm(C_EXTI);
}



/* CONTROL RATE TIMER INTERRUPT */

/* It seems simplest to run synchronous regulator and control code
   from a lower priority timer interrupt, and only use the lowest
   priority main loop for communication.  */

// FIXME: something is not right here...
#define CONTROL_DIV (72000 / 2)

static const struct hw_periodic hw_control_config[] = {
//          rcc       irq            tim   div          pre
//---------------------------------------------------------
    [3] = { RCC_TIM3, NVIC_TIM3_IRQ, TIM3, CONTROL_DIV, 1 },
    [4] = { RCC_TIM4, NVIC_TIM4_IRQ, TIM4, CONTROL_DIV, 1 },
    [5] = { RCC_TIM5, NVIC_TIM5_IRQ, TIM5, CONTROL_DIV, 1 },
};

#define C_CONTROL hw_control_config[TIM_CONTROL]


volatile uint32_t control_count;

void HW_TIM_ISR(TIM_CONTROL)(void) {
    hw_periodic_ack(C_CONTROL);
    control_count++;
}
void init_control(void) {
    /* Use another timer for the control loop, running at lower priority. */
    hw_periodic_init(C_CONTROL);
}



/* COMMUNICATION */

int handle_tag_u32(void *context,
                   const uint32_t *arg,  uint32_t nb_args,
                   const uint8_t *bytes, uint32_t nb_bytes) {
    if (nb_args < 1) return -1;
    switch(arg[0]) {
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


    // FIXME: init last_cc here?


    /* Smaller values mean higher priorities.  STM32F103 strips the
       low 4 bits, so these have increments of 16.  Print the actual
       value at boot to be sure. */
    NVIC_IPR(C_PDM.irq)       = 0;
    NVIC_IPR(NVIC_EXTI0_IRQ)  = 16;
    NVIC_IPR(C_CONTROL.irq)   = 32;
    infof("TIM_PDM     pri %d\n", NVIC_IPR(C_PDM.irq));
    infof("TIM_CONTROL pri %d\n", NVIC_IPR(C_CONTROL.irq));
    infof("EXTI0       pri %d\n", NVIC_IPR(NVIC_EXTI0_IRQ));

    infof("C_CONTROL.div = %d\n", C_CONTROL.div);

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

