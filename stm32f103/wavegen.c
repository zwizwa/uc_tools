// Wave form generator.
// This can serve as an example of how to use the ETF nested dictionary protocol.
// See etf_test.c for a simpler ETF protocol example.

#include "sm_etf.h"
#include "base.h"
#include "gdbstub_api.h"
#include <string.h>

#include "wavegen_tags.h"

// Configuration tables

#define DIV 1500 // (/ 72000000 48000)
#define TIM_PERIODIC 4
#define DBG_PIN GPIOA,3

/* Note that peripheral resource use isn't orthogonal in general: some
 * combinations are implemented, others are not.  To make this more
 * clear, the hw_*.h from uc_tools use configuration structs that can
 * be tabulated using const vectos.  These can then be combined with
 * macros and inline configuration functions to make make resource use
 * a bit clearer, and to make it simpler to switch resources during
 * development.  The C optimizer will eliminate most of the
 * overhead. */

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



/* The protocol consists of nested dictionaries, where the keys are
   numbers.  It allows hierarchical structuring of the parameter space
   on the other side (Erlang), where data structures are easy to use.

   At the C side, data structures are a bit more of a hassle, so
   sm_etf translates the hierachical message into successive calls to
   this function, where each call corresponds to one leaf value,
   annotated with a path.

   Paths (keys), can only be integer.  This allows straightforward
   mapping onto C's "pattern matching" construct which is the switch
   statement.

   Path keys should probably be symbolically named in a key file that
   only is appended to.

   The reply protocol used here is ad-hoc: newline-terminated printed
   erlang terms, which is easy to generate at this end, and easy to
   parse at the Erlang end.  At some point maybe raw ETF is better. */

uint32_t dispatch(uint8_t type,
                  uint8_t *buf, uint32_t buf_len,
                  int32_t *tag, uint32_t tag_len) {

    if (tag_len == 0) {
        // Untagged values are not supported.
        infof("{bad_tag_len,0}\nend\n");
        return 0;
    }
    if (tag_len == 1 && type == NIL_EXT) {
        infof("end\n");
        return 0;
    }
    if (tag_len != 1) {
        infof("{bad_tag_len,%d}\n",tag_len);
        return 0;
    }

    // As for payload, assume that the value is meaningful and is
    // representable as an unsigned integer.
    uint32_t val = as_uint32(type, buf, buf_len, -1);

    // What's left is to interpret the tag path as a variable name.
    // For now, only a single path depth is used.
    switch(tag[0]) {
    case tag_mode:
        if (val) { start_sampler(); }
        else     { stop_sampler();  }
        break;
    case tag_count:
        infof("{count,%d}\n", count); break;
    default:
        infof("{bad_tag,%d}\n",tag[0]); break;
    }
    return 0;
}


/* ETF IO */
struct sm_etf sm_etf;
uint8_t etf_buf[1024];

static uint32_t cb(struct sm_etf *sm) {
    return dispatch(sm->data_type, sm->buf, sm->data_size, sm->stack, sm->depth);
}
static void sm_etf_reset(void) {
    sm_etf_init(&sm_etf, &etf_buf[0], sizeof(etf_buf), &cb);
}
static void etf_write(const uint8_t *buf, uint32_t len) {
    uint32_t status = sm_etf_write(&sm_etf, buf, len);
    if (SM_WAITING != status) {
        infof("{parse_error,16#%x}\n",status);
        sm_etf_reset();
    }
}
static uint32_t etf_read(uint8_t *buf, uint32_t len) {
    return etf_tagged_read(123, info_read, buf, len);
}
const struct gdbstub_io etf_io = {
    .read  = etf_read,
    .write = etf_write,
};



/* STARTUP */
void switch_protocol(const uint8_t *buf, uint32_t size) {
    *_service.io = (struct gdbstub_io *)(&etf_io);
    (*_service.io)->write(buf, size);
}
void start(void) {
    hw_app_init();
    rcc_periph_clock_enable(RCC_GPIOA | RCC_AFIO);
    sm_etf_reset();
    hw_gpio_config(DBG_PIN,HW_GPIO_CONFIG_OUTPUT);
}
void stop(void) {
    hw_app_stop();
    _service.reset();
}
const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "Waveform Generator";
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;

struct gdbstub_config config CONFIG_HEADER_SECTION = {
    .manufacturer    = config_manufacturer,
    .product         = config_product,
    .firmware        = config_firmware,
    .version         = config_version,
    .start           = start,
    .stop            = stop,
    .switch_protocol = switch_protocol,
};

