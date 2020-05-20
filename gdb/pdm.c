#include <stdint.h>
#include "fixedpoint.h"


/* Pulse density modulator.

   This implements a DAC for controlling set points of analog
   audio circuits.  Currently we see roughly 3 solutions:

   - Resistor-ladder based Digital Analog Converter (DAC)

   - Pulse Width Modulation (PWM)

   - Pulse Density Modulation (PDM)

   Preliminary conclusion is that the DAC solution is too expensive as
   it requires extra hardware, the PWM solution has a noise spectrum
   that is too correlated for use in the application, so we end up
   with PDM as the only viable option.

   We start off with a first order PDM scheme, where we send out the
   carry bit of a discrete integrator.  I.e. with X the input to the
   PDM and A the accumulator, the output at any time instance is the
   carry bit of A = A + X with A and X limited to the desired
   resulution, which in our case is 16 bit.

   The noise spectrum of this modulation scheme strongly depends on
   the value of X.  Some examples to illustrate the idea.

   - If X is a power of 2, for a resolution of N also a power of 2,
     e.g. N=2^B where B is the number of bits of the accumulator, the
     output will be a periodic signal with one pulse every P samples:

       X     P
     ---------
       1     N
       2   N/2
       4   N/4
      N/2    2
       N     1

   - When X is coprime with N the period will be N, but there will
     still be a prominent spectral peak.  E.g. for N=8 and X=3, the
     sequence of A and carry bits is:

     A: 0 3 6 1 4 7 2 5 0 ...
     C: 1 0 0 1 0 0 1 0 1 ...

     Note the complementary pattern when X = 5 = 8 - 3 produces the
     same waveform, but in reverse.

     A: 0 5 2 7 4 1 6 3 0 ...
     C: 1 0 1 0 0 1 0 0 1 ...

     Both have a dominant peak around 3/8.

     TODO: Make this more precise, or include some plots.

   Now, what we want to accomplish is to move that dominant peak up
   into higher frequences.  A very crude and simple way to do this is
   to not use part of the range.  The noise will only appear in the
   lower parts of the frequency spectrum if X is small or close to N
   (i.e. negative X is small).

   The approach we take is to ensure that we do not have to use the
   lower and upper 25 percent of the range of X.  So we sacrifice one
   bit of precision, in order to keep the noise period above 1/4th of
   the update frequency.

   This brings us to the first design parameter: requiring the
   dominant peak of the noise spectrum to be be above the audio range,
   e.g. >20kHz, we only need to oversample 4x, so we can set the
   update frequency in the range of 80kHz.

*/

#define PDM_DIV (72000 / 200)

/* The circuit is designed such that:

   - Operation modes are save regardless of how the modulator is
     driven.

   - The usable range is between 25% and 75% modulation to keep the
     modulation frequency high, as explained above.

   All setpoints assignments go through this function.  It can later
   be clipped is necessary, but since the circuit is safe over the
   entire range, that might not be necessary.
*/
#define SETPOINT_MIN 0x40000000ULL
#define SETPOINT_MAX 0xC0000000ULL
uint32_t safe_setpoint(uint32_t setpoint) {
#if 0
    if (setpoint < SETPOINT_MIN) return SETPOINT_MIN;
    if (setpoint > SETPOINT_MAX) return SETPOINT_MAX;
#endif
    return setpoint;
}


/*
   The other constraint is that the output noise spectrum will still
   be quoite periodic, which might cause issues when modulation is
   involved, as the noise spectrum can end up back into the audio
   spectrum.  The way around this is to add some form of dithering
   into the signal.  We will (TODO: later) insert this as a
   pseudo-random signal in the low bit of the input to the PDM that
   averages to 0.
*/


/* PLATFORM SPECIFIC */

/* CONFIGURATION */
/* First channel pin.  Other channels are adjacent, incrementing. */
#define PDM_PIN_FRAME 3
#define PDM_PIN_CHAN0 4
/* All channels are part of one port. */
#define PDM_PORT GPIOA
/* This sets the number of channels.  Used for struct gen and code gen. */
#define FOR_CHANNELS(c) \
    c(0) c(1) c(2) c(3) \
    c(4) c(5) c(6) c(7) \


#include "base.h"
#include "gdbstub_api.h"
#include <string.h>


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



/* Protocol-wise we don't really need anything special, so use
   slipstub_buffered to do the basics (SLIP framing, PING, GDB, INFO)
   and use tag_u32 for app-specific commands. */
#include "tag_u32.h"
#include "slipstub.h"
struct slipstub slipstub;
struct slipstub_buffers slipstub_buffers;


/* Don't use TIM2. It interacts badly with Flash programming. */
#define TIM_PDM      4
#define TIM_CONTROL  3

static const struct hw_periodic hw_pdm_config[] = {
//          rcc       irq            tim   div      pre
//-------------------------------------------------------
    [3] = { RCC_TIM3, NVIC_TIM3_IRQ, TIM3, PDM_DIV, 1 },
    [4] = { RCC_TIM4, NVIC_TIM4_IRQ, TIM4, PDM_DIV, 1 },
    [5] = { RCC_TIM5, NVIC_TIM5_IRQ, TIM5, PDM_DIV, 1 },
};

#define C_PDM hw_pdm_config[TIM_PDM]

void start_modulator(void) {
    infof("start\n");
    hw_periodic_init(C_PDM);
}
void stop_modulator(void) {
    infof("stop\n");
    hw_periodic_disable(C_PDM);
}

struct channel {
    uint32_t setpoint;
    uint32_t accu;
};
#define CHANNEL_STRUCT(c) {},
struct channel channel[] = { FOR_CHANNELS(CHANNEL_STRUCT) };

#define NB_CHANNELS (sizeof(channel) / sizeof(struct channel))




/* The modulator will need to deliver a pulse whenver the accumulator
   overflows.  ARM can shift LSB and MSB using ADC and RRX
   respectively. */

INLINE void channel_update(
    struct channel *channel,
    uint32_t *shiftreg) {

    __asm__ (
        "   adds %0, %0, %2  \n"   // update accu, update carry
      //"   adc  %1, %1, %1  \n"   // shift carry flag into LSB
        "   rrx %1, %1       \n"   // shift carry flag into MSB
        : "+r"(channel->accu),     // %0 read/write
          "+r"(*shiftreg)          // %1 read/write
        :  "r"(channel->setpoint)  // %2 read
        : );
}

#define CHANNEL_UPDATE(c) \
    channel_update(&channel[c], &shiftreg);

#define DEBUG_MARK \
    __asm__ volatile ( \
        "   nop             \n" \
        "   nop             \n" \
        "   nop             \n" \
        : )

INLINE uint32_t channels_update(void) {
    uint32_t shiftreg = 0;
    FOR_CHANNELS(CHANNEL_UPDATE);
    return shiftreg;
}


/* PDM TIMER INTERRUPT */

static volatile uint32_t bsrr_last = 0;

void HW_TIM_ISR(TIM_PDM)(void) {
    hw_periodic_ack(C_PDM);

    GPIO_BSRR(PDM_PORT) = (1 << PDM_PIN_FRAME);

    //DEBUG_MARK;

    /* Assume GPIOs are contiguous.  We're using RRX to shift into MSB
     * to keep the pin order the same as the channel order. */
    uint32_t set_bits = channels_update() >> (32 - NB_CHANNELS - PDM_PIN_CHAN0);
    uint32_t mask     = ((1 << NB_CHANNELS) - 1) << PDM_PIN_CHAN0;
    uint32_t clr_bits = (~set_bits) & mask;
    uint32_t bsrr     = set_bits  | (clr_bits << 16) | (0x10000 << PDM_PIN_FRAME);
    GPIO_BSRR(PDM_PORT) = bsrr;

    // Log last non-trivial set/clear command
    // if (set_bits) { bsrr_last = bsrr; }

    //DEBUG_MARK;
}


/* OSCILLATOR PULSE INTERRUPT */

const struct hw_exti c_exti = HW_EXTI_A0_B;
#define C_EXTI c_exti
#define C_GPIO GPIOA,0
volatile uint32_t nb_pulses, last_cc, osc_period;

/* Use the ARM 32-bit cycle counter to do time stamping.  It's more
   convenient than having to work around 16bit counter limitations.
   The 16 bit timers could later be used as oscillators. */

#include <libopencm3/cm3/dwt.h>
#define DEMCR  MMIO32(0xE000EDFC)
#define LAR    MMIO32(0xE0001FB0)

// https://stackoverflow.com/questions/36378280/stm32-how-to-enable-dwt-cycle-counter
static inline void enable_cycle_counter(void) {
    DEMCR |= 0x01000000;    // enable trace
    LAR = 0xC5ACCE55;       // <-- added unlock access to DWT (ITM, etc.)registers
    DWT_CYCCNT = 0;         // clear DWT cycle counter
    DWT_CTRL |= 1;          // enable DWT cycle counter
}
static inline uint32_t cycle_counter(void) {
    return DWT_CYCCNT;
}




void exti0_isr(void) {
    hw_exti_ack(C_EXTI);
    if (hw_gpio_read(C_GPIO)) {
        /* Time base is ARM cycle counter @72MHz.  Sample this early
           in the ISR to avoid any variable delay. */
        uint32_t cc = cycle_counter();

        /* New measurement is the elapsed time since last capture. */
        uint32_t osc_period_new = cc - last_cc;

        /* We filter that using a first order filter.  Note that this
           is updated at the oscilattor's current rate, so the filter
           pole depends on the frequency of the oscillator. */
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




/* COMMUNICATION */

int handle_tag_u32(void *context,
                   const uint32_t *arg,  uint32_t nb_args,
                   const uint8_t *bytes, uint32_t nb_bytes) {
    if (nb_args < 1) return -1;
    switch(arg[0]) {
    case 1: // MODE
        //  bp2 ! {send_packet, <<16#FFF50002:32, 1:32, 1:32}.
        if (nb_args < 2) return -1;
        if (arg[1]) { start_modulator(); }
        else        { stop_modulator(); }
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
    rcc_periph_clock_enable(RCC_GPIOA | RCC_AFIO);

    /* PDM frame clock.  This is set/reset at beginning/end of PDM
       ISR, so can be used as a CPU usage measurement. */
#ifdef PDM_PIN_FRAME
    hw_gpio_config(
        PDM_PORT,
        PDM_PIN_FRAME,
        HW_GPIO_CONFIG_OUTPUT);
#endif

    /* PDM outputs, one per channel.  Note that these really need to
       go through some digital cleanup, e.g. an inverter or a buffer
       that is on a stable supply, before being fed into the analog
       low pass filter.  I've found the STM32F103 outputs to be quite
       noisy, and there is variable voltage drop depending on whether
       the chip is driving high current through other I/Os,
       e.g. driving an ELED.  */
    for(int i=0; i<NB_CHANNELS; i++) {
        infof("port %x pin %d\n", PDM_PORT, PDM_PIN_CHAN0 + i);
        hw_gpio_config(
            PDM_PORT,
            PDM_PIN_CHAN0 + i,
            // HW_GPIO_CONFIG_OPEN_DRAIN_2MHZ
            HW_GPIO_CONFIG_OUTPUT
            );
    }

    /* Use framwork for handling incoming USB SLIP commands. */
    slipstub_init(handle_tag);

    /* Move all setpoints into a safe range before starting the
       modulator. */
    for(int i=0; i<NB_CHANNELS; i++) {
        channel[i].setpoint = safe_setpoint(0x40000000ULL);
    }
    channel[0].setpoint = 2000000000;
    start_modulator();


    /* External input interrupt for discharge pulse.  This needs to be
       lower priority than the PDM because it will cause modulation
       effects. */
#if 1
    enable_cycle_counter();
    hw_exti_init(C_EXTI);
    hw_exti_arm(C_EXTI);
#endif

    /* Use another timer for the control loop. */
    hw_periodic_init(C_CONTROL);

    /* Smaller values mean higher priorities.  STM32F103 strips the
       low 4 bits, so these have increments of 16.  Print the actual
       value at boot to be sure. */
    NVIC_IPR(C_PDM.irq)       = 0;
    NVIC_IPR(C_CONTROL.irq)   = 16;
    NVIC_IPR(NVIC_EXTI0_IRQ)  = 32;
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

