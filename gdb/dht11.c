/* DHT11 interface

   Note that this is much easier to implement as a blocking task, and
   usually I would do so.  The 18ms time delay isn't a problem for
   pausing the USB.

   However, this was too nice an example to not use as a motivation to
   explore event driven programming in a cleaner way, where events are
   abstracted and isolated hardware-dependent code implements the
   event propagation and setup.  This makes event-driven code
   testable.

   The hardware used in this example is:
   - A periodic interrupt to provide time-based events
   - EXTI GPIO edge detection
   - Direct GPIO contrl
   - Free-running timer readout

*/

#include "base.h"
#include "infof.h"


/* The generic DHT11 driver is prameterized by platform dependent
 * functionality in terms of inline functions. */
#include "dht11.h"
struct dht11 dht11;
/* Instantiate the event handler. */
static void send(uint32_t event) {
    dht11_handle(&dht11, event);
}


/* A single general-purpose timer is used to provide two time-based
   mechanisms at different time scales:

   - The ability to measure us-level time differences for decoding
     pulse width modulation.

   - A periodic interrupt to schedule ms-level events such as the
     request pulse, and an overall timeout.

   Time differences are simplest to perform and have the highest
   accuracy when the timer runs over the entire 16 bit range.  That
   sets div=0x10000 and gives a base clock of

   (/ 72000.0 #x10000) 1.1 kHz or 0.9us

   This fits perfectly, so the prescal can be set to 1.
*/


const struct hw_periodic hw_tim_10us[] = {
//          rcc       irq            tim   div      pre
//-------------------------------------------------------
    [2] = { RCC_TIM2, NVIC_TIM2_IRQ, TIM2, 0x10000, 1 },
};
#define TIM 2
#define C_TIM hw_tim_10us[TIM]

/* The hardware timer values -- i.e. the fractional number of 1.1kHz
   ticks -- is used for fine scale measurements. */
volatile uint16_t timer_frac_mark;
static inline void dht11_hw_time_reset(struct dht11 *s) {
    timer_frac_mark = hw_tim_counter(C_TIM.tim);
}
static inline uint32_t dht11_hw_time_us(struct dht11 *s) {
    uint16_t t = hw_tim_counter(C_TIM.tim) - timer_frac_mark;
    return t; // FIXME: SCALE
}

/* The integral number of 1.1kHz ticks is used for delay events. */
volatile uint32_t timer_ticks;
volatile uint32_t timer_delay_mark;
volatile uint32_t timer_delay_enable;
void HW_TIM_ISR(TIM)(void) {
    hw_periodic_ack(C_TIM);
    uint32_t ticks = timer_ticks++;
    if (timer_delay_enable && (timer_delay_mark == ticks)) {
        timer_delay_enable = 0;
        send(DHT11_EVENT_DELAY);
    }
}
static inline void dht11_hw_delay_start(struct dht11 *s, uint32_t us) {
    timer_delay_enable = 1;
    timer_delay_mark = timer_ticks + us; // FIXME: SCALE
}
static inline void dht11_hw_delay_stop(struct dht11 *s) {
    // Just leave it on for now
}


/* EXTI and GPIO interfaces. */
const struct hw_exti c_exti = HW_EXTI_A0_B;
#define C_EXTI c_exti
#define C_GPIO GPIOA,0
volatile uint32_t count_exti;
void exti0_isr(void) {
    hw_exti_ack(C_EXTI);
    uint32_t event = hw_gpio_read(C_GPIO) ?
        DHT11_EVENT_POSEDGE :
        DHT11_EVENT_NEGEDGE;
    send(event);
    count_exti++;
}
static inline void dht11_hw_io_enable(struct dht11 *s) {
    // Always on
}
static inline void dht11_hw_io_disable(struct dht11 *s) {
    // FIXME: always on
}
// Weak 1, strong 0.
static inline void dht11_hw_io_write(struct dht11 *s, int val) {
    if (val) {
        hw_gpio_write(C_GPIO,1); // pull up direction
        hw_gpio_config(C_GPIO, HW_GPIO_CONFIG_INPUT_PULL);
    }
    else {
        hw_gpio_write(C_GPIO,0);
        hw_gpio_config(C_GPIO, HW_GPIO_CONFIG_OUTPUT);
    }
}


/* Endpoint */
static inline void dht11_hw_response(struct dht11 *s, uint8_t rh, uint8_t t) {
    infof("dht11: %d %d\n", rh, t);
    /* Resources can be freed here */

    /* FIXME: The machine is halted.  All interrupts can be turned
     * off, but the machine will ignore them if they still arrive. */
}




#include <stdint.h>
#include <string.h>

#include "gdbstub_api.h"
#include <string.h>
#include "cbuf.h"

struct cbuf to_usb; uint8_t to_usb_buf[1024];

/* For debugging. */
volatile uint32_t count_timer = 0;
volatile uint32_t count_exti = 0;



static void app_write(const uint8_t *buf, uint32_t len) {
    //infof("app_write: %d\n", len);
}
static uint32_t app_read(uint8_t *buf, uint32_t len) {
    return cbuf_read(&to_usb, buf, len);
    //return info_read(buf, len);
}
const struct gdbstub_io app_io = {
    .read  = app_read,
    .write = app_write,
};
void switch_protocol(const uint8_t *buf, uint32_t size) {
    *_service.io = (struct gdbstub_io *)(&app_io);
    (*_service.io)->write(buf, size);
}






#if 1
/* Interactive testing at GDB prompt. */
KEEP void test(int n) {
    switch(n) {
    case 0:
        dht11_request(&dht11);
        break;
    }
}
#endif



const char config_product[];
void start(void) {
    /* Low level application init.  Note that this needs to be called
     * manually after loading to initialize memory. */
    hw_app_init();
    CBUF_INIT(to_usb);
    uint8_t msg[] = {0,0,0,1,123};
    cbuf_write(&to_usb, msg, sizeof(msg));

    /* Use a single periodic timer to provide time base.  If the
     * application allows for it -- basically a power consumption
     * requirement because CPU will do more work -- this is almost
     * always simpler than messing with timer configurations
     * directly. */
    hw_periodic_init(C_TIM);

    /* GPIO & EXTI */
    rcc_periph_clock_enable(RCC_GPIOA | RCC_AFIO);
    hw_gpio_high(GPIOA,0); // pull up
    hw_gpio_config(GPIOA,0,HW_GPIO_CONFIG_INPUT_PULL);
    hw_exti_init(C_EXTI);
    hw_exti_arm(C_EXTI);

    infof("product: %s\n",&config_product[0]);

}

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "DHT11 interface board";
//const char config_serial[]       CONFIG_DATA_SECTION = "123";
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;
const char config_version[]      CONFIG_DATA_SECTION = BUILD;
const char config_protocol[]     CONFIG_DATA_SECTION = "{packet,4}";

struct gdbstub_config config CONFIG_HEADER_SECTION = {
    .manufacturer    = config_manufacturer,
    .product         = config_product,
    //.serial          = config_serial,
    .firmware        = config_firmware,
    .version         = config_version,
    //.protocol        = config_protocol,
    .start           = start,
    .switch_protocol = switch_protocol,
};






