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

   The appriach we take is to ensure that we do not have to use the
   lower and upper 25 percent of the range of X.  So we sacrifice one
   bit of precision, in order to keep the noise period above 1/4th of
   the update frequency.

   This brings us to the first design parameter: requiring the
   dominant peak of the noise spectrum to be be above the audio range,
   e.g. >20kHz, we only need to oversample 4x, so we can set the
   update frequency in the range of 80kHz.

*/

#define DIV 900 // (/ 72000000 (* 20000 4))

/*
   The other constraint is that the output noise spectrum will still
   be quote periodic, which might cause issues when modulation is
   involved, as the noise spectrum can end up back into the audio
   spectrum.  The way around this is to add some form of dithering
   into the signal.  We will (TODO: later) insert this as a
   pseudo-random signal in the low bit of the input to the PDM that
   averages to 0.
*/


/* PLATFORM SPECIFIC */


#include "base.h"
#include "gdbstub_api.h"
#include <string.h>

/* Protocol-wise we don't really need anything special, so use
   slipstub_buffered to do the basics (SLIP framing, PING, GDB, INFO)
   and use tag_u32 for app-specific commands. */
#include "tag_u32.h"
#include "slipstub.h"
struct slipstub slipstub;
struct slipstub_buffers slipstub_buffers;


#define TIM_PERIODIC 4
#define DBG_PIN GPIOA,3

static const struct hw_periodic hw_periodic_config[] = {
//          rcc       irq            tim   div  pre
//---------------------------------------------------
    [2] = { RCC_TIM2, NVIC_TIM2_IRQ, TIM2, DIV, 1 },
    [3] = { RCC_TIM3, NVIC_TIM3_IRQ, TIM3, DIV, 1 },
    [4] = { RCC_TIM4, NVIC_TIM4_IRQ, TIM4, DIV, 1 },
    [5] = { RCC_TIM5, NVIC_TIM5_IRQ, TIM5, DIV, 1 },
};

#define C_PERIODIC hw_periodic_config[TIM_PERIODIC]

void start_sampler(void) {
    infof("start\n");
    hw_periodic_init(C_PERIODIC);
}
void stop_sampler(void) {
    infof("stop\n");
    hw_periodic_disable(C_PERIODIC);
}
static volatile uint32_t count = 0;
void HW_TIM_ISR(TIM_PERIODIC)(void) {
    /* Issiuing ack at the end of the isr will re-trigger it. What is
    the delay necessary to avoid that?  Issuing it at the beginning
    apparently creates enough of a delay. */
    hw_periodic_ack(C_PERIODIC);

    hw_gpio_write(DBG_PIN,count&1);
    count++;
}

int handle_tag_u32(void *context,
                   const uint32_t *arg,  uint32_t nb_args,
                   const uint8_t *bytes, uint32_t nb_bytes) {
    if (nb_args < 1) return -1;
    switch(arg[0]) {
    default:
        infof("unknown command:");
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
    rcc_periph_clock_enable(RCC_GPIOA | RCC_AFIO);
    hw_gpio_config(DBG_PIN,HW_GPIO_CONFIG_OUTPUT);
    slipstub_init(handle_tag);
}
void stop(void) {
    hw_app_stop();
    _service.reset();
}
const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "PDM Test";
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;
const char config_version[]      CONFIG_DATA_SECTION = BUILD;
const char config_protocol[]     CONFIG_DATA_SECTION = "slip";

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

