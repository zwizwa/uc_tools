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

/* Init is at the end here to allow non-scalar parameters.  Ideally,
   code at the other end will use symbolic names, and not parameter
   ids. */

#define FOR_PARAMETERS(p) \
    p(0, uint32,   osc_setpoint, "Main oscillator setpoint, in 5.27 nlog2(period_cycles)", (15 << 27)) \

union types {
    uint32_t   uint32;
};
#define DEF_PARAMETER(_num, _type, _name, _info, _init) \
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
#define DEF_PARAMETER_INFO(_num, _type, _name,  _info, _init)           \
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

struct period_measurement {
    /* Config */
    uint32_t log_max; // max = 1<<log_max

    /* User state */
    uint32_t write;   // low bit points to current measurement
    uint32_t read;    // ..
    uint32_t avg[2];  // last masurement with (32-log_max) fractional bits

    /* For ISR only. */
    uint32_t num;   // number of measurements
    uint32_t accu;  // current sum
};


volatile struct period_measurement period_measurement = {
    .log_max = 26, /* One second is about 1<26 cycles. */
};

/* This buffer stores continuations for reply messages.  E.g. pids of
   waiting erlang tasks.  An Erlang pid is 27 bytes, plus one byte for
   the size.  So this can fit two pids. */
struct cbuf measurement_wait; uint8_t measurement_wait_buf[64]; // FIXME

void period_measurement_poll(void) {
#if 1
    volatile struct period_measurement *p = &period_measurement;

    if (p->read != p->write) {
        // FIXME: make sure they are equal after incrementing.  The
        // buffer is only one deep so other variables are not valid.
        uint32_t read = ++p->read;
        uint32_t avg = p->avg[read&1];
        (void)avg;
        //infof("period_avg: %d\n", avg);

        /* Poll the waiter queue. */
        struct cbuf *b = &measurement_wait;
        struct cbuf *o = slipstub.slip_out;

#if 0
        uint16_t w;
        infof("cbuf:");
        while (CBUF_EAGAIN != (w = cbuf_get(b))) {
            infof(" %d", w);
        }
        infof("\n");
#endif

#if 1
        if (cbuf_bytes(b)) {
            uint16_t len  = cbuf_get(b);
            uint32_t have = cbuf_bytes(b);
            if (len < have) {
                infof("bad measurement_wait size %d, %d. clearing.\n",
                      cbuf_bytes(b), len);
                cbuf_clear(b);
            }
            else {
                infof("sending measurement reply: avg=%d, cont=%d\n", avg, len);
                uint8_t h[] = { U16_BE(TAG_REPLY) };
                uint8_t k[len]; cbuf_read(b, k, len);
                uint8_t a[] = { U32_BE(avg) };
                CBUF_WRITE_3(o, h, k, a);
            }
        }
#endif
    }
#endif
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

    /* Current period measurement. */
    uint32_t meas = cc - last_cc;
    last_cc = cc;


    /* Update average.  The oscillator is quite unstable, so we need
       to measure for a long time if we want a good reading (e.g. for
       about a second).  */
    volatile struct period_measurement *p = &period_measurement;
    uint32_t accu = p->accu;
    uint32_t accu1 = accu + meas;
    uint32_t log_max = p->log_max;
    uint32_t max = 1 << log_max;
    if (accu1 < max) {
        p->num++;
        p->accu = accu1;
    }
    else {
        /* Double buffering is used to pass data to the low rate task.
           It can sync on p->write changing and pick up
           p->avg[p->write & 1]. */
        uint32_t write = p->write + 1;
        uint32_t avg = (accu << (32-log_max)) / p->num;
        p->avg[write & 1] = avg;
        p->write = write;
        p->num = 1;
        p->accu = meas;
    }

    /* Note that it took a while to realize that there are two
       problems to solve: calibration of static components, and
       compensation of temperature variations.  The temperature
       variations are slow, so we can use a fairly slow control
       algorithm for that, which allows us to focus on taking our time
       to get precise measurements. */
}

void init_osc_in(void) {
    /* External input interrupt for discharge pulse.  This needs to be
       lower priority than the PDM because it will cause modulation
       effects. */
    CBUF_INIT(measurement_wait);
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


#if 0
// FIXME: Remove?  This now uses TAG_U32
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
#endif



/* Ad-hoc multi-argument messages are not in the table. */
int handle_tag_u32(void *context,
                   const uint32_t *arg,  uint32_t nb_args,
                   const uint8_t *bytes, uint32_t nb_bytes) {
    if (nb_args < 1) return -1;
    switch(arg[0]) {

        /* Ad hoc commands that do not fit the simple parameter table
           structure. */

    case 100: { // MODE
        //  bp2 ! {send_packet, <<16#FFF50002:32, 100:32, 1:32}.
        if (nb_args < 2) return -1;
        if (arg[1]) { pdm_start(); }
        else        { pdm_stop(); }
        return 0;
    }
    case 101: { // SETPOINT
        struct { uint32_t cmd; uint32_t chan; uint32_t val; } *a = (void*)arg;
        if (nb_args < 3) return -1;
        if (a->chan >= NB_CHANNELS) return -2;
        channel[a->chan].setpoint = safe_setpoint(a->val);
        infof("setpoint[%d] = %x\n", a->chan, channel[a->chan].setpoint);
        return 0;
    }
    case 102: { // MEASURE
        if (nb_bytes) {
            // Binary payload is the continuation.  The following
            // measurement result will be forwarded.
            infof("measurement_wait queue: %d\n", nb_bytes);
            struct cbuf *b = &measurement_wait;
            cbuf_put(b, nb_bytes);
            cbuf_write(b, bytes, nb_bytes);
        }
        return 0;
    }
    case 103: { // INFO
        //infof("bsrr_last = %x\n", bsrr_last);
        for (int i=0; i<NB_CHANNELS; i++) {
            infof("%d: %x %x\n", i, channel[i].accu, channel[i].setpoint);
        }
        infof("osc_period: %d\n", osc_period);
        infof("nb_pulses:  %d\n", nb_pulses);
        infof("last_cc:    %d\n", last_cc);
        return 0;
    }

    default:
        /* Simple parameters can be specified in the parameter table
           and don't need much protocol handling. */
        if ((nb_args  == 2) &&
            (nb_bytes == 0) &&
            (arg[0] < ARRAY_SIZE(parameter_info))) {
            struct parameter_info *pi = &parameter_info[arg[0]];
            // FIXME: this assumes endianness allows this.
            pi->data->uint32 = arg[1];
            infof("%d: %s = %d\n", arg[0], pi->name, arg[1]);
            return 0;
        }
        infof("unknown tag_u32 command:");
        for (int i=0; i<nb_args; i++) { infof(" %d", arg[i]); }
        infof("\n");
        return -2;
    }


#if 0

    /* Old test code */

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
}

/* slipstub calls this one for application tags.  We then patch
   through to command handler. */
void handle_tag(struct slipstub *s, uint16_t tag, const struct pbuf *p) {
    infof("tag %d\n", tag);
    switch(tag) {
    case TAG_U32: {
        /* name ! {send_u32, [101, 1000000000, 1,2,3]}. */
        int rv = tag_u32_dispatch(handle_tag_u32, NULL, p->buf, p->count);
        if (rv) { infof("tag_u32_dispatch returned %d\n", rv); }
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
    _service.add(period_measurement_poll);

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

