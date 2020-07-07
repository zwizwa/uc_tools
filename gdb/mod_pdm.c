#ifndef MOD_PDM_C
#define MOD_PDM_C

/* Pulse density modulator.

   This implements a DAC for controlling set points of analog
   audio circuits.  Currently we see roughly 3 solutions:

   - Resistor-ladder based Digital Analog Converter (DAC)

   - Pulse Width Modulation (PWM)

   - Pulse Density Modulation (PDM)

   Preliminary conclusion is that the DAC solution is too expensive as
   it requires extra hardware, the PWM solution has a noise spectrum
   that is too correlated for use in the application when run at low
   rate, and when running from hardware the number of modulators is
   limited, so we end up with bit-banged PDM as a reasonable option.

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


#include "xorshift.h"



/* The circuit is designed such that:

   - Operation modes are safe regardless of how the modulator is
     driven.

   - The usable range is between 25% and 75% modulation to keep the
     modulation frequency high, as explained above.

   All setpoints assignments go through this function.  It can later
   be clipped if necessary.  The original circuit was not safe over
   the entire range so relied on this to be set carefully.

   Current hardware design does not have an unsafe setting.  For
   settings that are too high, a base resistor will limit current.
   This introduces a non-linearity that can be conmpensated for.

*/
#define PDM_SETPOINT_MIN 0x40000000ULL
#define PDM_SETPOINT_MAX 0xC0000000ULL
uint32_t pdm_safe_setpoint(uint32_t setpoint) {
#if 0
    if (setpoint < PDM_SETPOINT_MIN) return PDM_SETPOINT_MIN;
    if (setpoint > PDM_SETPOINT_MAX) return PDM_SETPOINT_MAX;
#endif
    return setpoint;
}



/* PLATFORM SPECIFIC */

/* CONFIGURATION */

/* First channel pin.  Other channels are adjacent, incrementing. */
#define PDM_PIN_CHAN0 4

/* All channels are part of one port. */
#define PDM_PORT GPIOA

#define PDM_CPU_USAGE_MARK GPIOA,3

/* This sets the number of channels.  Used for struct gen and code gen. */
#define PDM_FOR_CHANNELS(c) \
    c(0) c(1) 

// c(2) c(3) c(4) c(5) c(6) c(7)

static inline void     pdm_update(void);
static inline uint32_t pwm_update(void);

static inline void     control_trigger(void);

/* Don't use TIM2. It interacts badly with Flash programming. */
#define TIM_PDM      3
#define TIM_CONTROL  4

/* PDM, and additionally provide a single fixed rate PWM channel,
   e.g. for digital effects. */

const struct hw_clockgen hw_pdm_config[] = {
//          rcc_tim   rcc_gpio   tim   gpio   pin div       duty        phase  pol chan     itr  irq (optional)
//-------------------------------------------------------------------------------------------------------------------
    [3] = { RCC_TIM3, RCC_GPIOB, TIM3, GPIOB, 0,  PDM_DIV,  PDM_DIV/2,  0,     1,  TIM_OC3, -1,  NVIC_TIM3_IRQ },

};
#define C_PDM hw_pdm_config[TIM_PDM]

void pdm_start(void) {
    infof("start\n");
    hw_clockgen_init(C_PDM);
    hw_clockgen_arm(C_PDM);
    hw_clockgen_trigger(C_PDM);
}
void pdm_stop(void) {
    infof("stop: FIXME: not implemented\n");
}

#define OSC_HARD_SYNC() {pwm_phase = 0;}
volatile uint32_t pwm_phase = 0;
volatile uint32_t pwm_speed = 256 * 13;
#define PHASE_MASK 0xFFFFFF

#define CONTROL_DIV 256
uint32_t control_div_count = 0;

static inline uint32_t pwm_update(void) {
    // FIXME: Currently this is just a SAW, but it should be the
    // FM/Wavetable part.
    uint32_t phase = pwm_phase;
    uint32_t duty = phase >> 16;
    phase = (phase + pwm_speed + (phase >> 9)) & PHASE_MASK;
    pwm_phase = phase;
    return duty;
}

void HW_TIM_ISR(TIM_PDM)(void) {
    hw_clockgen_ack(C_PDM);
    hw_gpio_high(PDM_CPU_USAGE_MARK);

    pdm_update();
    uint32_t val = pwm_update();
    hw_clockgen_duty(C_PDM, val);

    if (control_div_count == 0) {
        /* Swap buffers: previously computed control values are now
           used in main PDM/PWM interrupt, and the control interrupt
           can start a new update. */
        // FIXME
        control_trigger();
    }
    control_div_count = (control_div_count + 1) % CONTROL_DIV;
    hw_gpio_low(PDM_CPU_USAGE_MARK);
}



struct channel {
    uint32_t setpoint;
    uint32_t accu;
};
#define CHANNEL_STRUCT(c) {},
struct channel pdm_channel[] = { PDM_FOR_CHANNELS(CHANNEL_STRUCT) };

#define PDM_NB_CHANNELS (sizeof(pdm_channel) / sizeof(struct channel))




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

/* Same, but add a dither signal.  This is computed in the main loop
   from a bit-limited xorshift signal. */
INLINE void channel_update_dither(
    struct channel *channel,
    uint32_t *shiftreg,
    uint32_t dither) {

    uint32_t dither_setpoint = channel->setpoint + dither;
    __asm__ (
        "   adds %0, %0, %2  \n"   // update accu, update carry
      //"   adc  %1, %1, %1  \n"   // shift carry flag into LSB
        "   rrx %1, %1       \n"   // shift carry flag into MSB
        : "+r"(channel->accu),     // %0 read/write
          "+r"(*shiftreg)          // %1 read/write
        :  "r"(dither_setpoint)    // %2 read
        : );
}


// #define PDM_CHANNEL_UPDATE(c) channel_update(&channel[c], &shiftreg);
#define PDM_CHANNEL_UPDATE(c) \
    channel_update_dither(\
        &pdm_channel[c], &shiftreg, dither);

#define ASM_DEBUG_MARK \
    __asm__ volatile ( \
        "   nop             \n" \
        "   nop             \n" \
        "   nop             \n" \
        : )

INLINE uint32_t pdm_channels_update(void) {
    uint32_t shiftreg = 0;
    uint32_t dither = random_u32() & 0x0FFFFFFF;
    PDM_FOR_CHANNELS(PDM_CHANNEL_UPDATE);
    return shiftreg;
}


/* PDM TIMER INTERRUPT */

static volatile uint32_t bsrr_last = 0;

static inline void pdm_update(void) {
    //ASM_DEBUG_MARK;

    /* Assume GPIOs are contiguous.  We're using RRX to shift into MSB
     * to keep the pin order the same as the channel order. */
    uint32_t set_bits = pdm_channels_update() >> (32 - PDM_NB_CHANNELS - PDM_PIN_CHAN0);
    uint32_t mask     = ((1 << PDM_NB_CHANNELS) - 1) << PDM_PIN_CHAN0;
    uint32_t clr_bits = (~set_bits) & mask;
    uint32_t bsrr     = set_bits  | (clr_bits << 16);
    GPIO_BSRR(PDM_PORT) = bsrr;

    // Log last non-trivial set/clear command
    // if (set_bits) { bsrr_last = bsrr; }

    //ASM_DEBUG_MARK;
}



void pdm_init(void) {
    /* PDM frame clock.  This is set/reset at beginning/end of PDM
       ISR, so can be used as a CPU usage measurement. */
    hw_gpio_config(
        PDM_CPU_USAGE_MARK,
        HW_GPIO_CONFIG_OUTPUT);

    /* PDM outputs, one per channel.  Note that these really need to
       go through some digital cleanup, e.g. an inverter or a buffer
       that is on a stable supply, before being fed into the analog
       low pass filter.  I've found the STM32F103 outputs to be quite
       noisy, and there is variable voltage drop depending on whether
       the chip is driving high current through other I/Os,
       e.g. driving an LED.  */

    /* The output filter here is important.  The average is very
       sensitive to the transients of the PDM signal, especially
       noticable on breadboard with wires going all over the place.
       With 50MHz I am able to hear half a semitone when I touch the
       5cm long wire. */

    for(int i=0; i<PDM_NB_CHANNELS; i++) {
        infof("port %x pin %d\n", PDM_PORT, PDM_PIN_CHAN0 + i);
        hw_gpio_config(
            PDM_PORT,
            PDM_PIN_CHAN0 + i,
            // HW_GPIO_CONFIG_OUTPUT
            HW_GPIO_CONFIG_OUTPUT_2MHZ
            );
    }

    /* Move all setpoints into a safe range before starting the
       modulator. */
    for(int i=0; i<PDM_NB_CHANNELS; i++) {
        pdm_channel[i].setpoint = pdm_safe_setpoint(0x40000000ULL);
    }
    pdm_channel[0].setpoint = 2000000000;





}



#if 0
    /* Old test code fragments */

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
#endif



#endif
