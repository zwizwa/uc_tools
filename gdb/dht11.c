/* DHT11 interface

   Note that this is much easier to implement using
   blocking/busywaiting.

   However, this was an opportunity to explore event driven
   programming in a cleaner way, where events are abstracted and
   isolated hardware-dependent code implements the event propagation
   and setup.  This makes event-driven code testable.

   The hardware used in this example is:
   - A periodic interrupt to provide time-based events
   - Free-running timer readout
   - EXTI GPIO edge detection
   - GPIO read/write

   The there is an additional control for a high side PNP switch to
   feed the device on the data line through a 22Ohm resistor.  We turn
   that off when the device sends a message. A 100uF cap + diode is
   enough to bridge the communication time (20ms), causing about a
   400mV drop in VCC down from about 4.2V.  Ask Tom for schematics.

*/

#include "base.h"
#include "infof.h"
#include "slipstub.h"

/* Buffers for SLIP I/O.  Input uses libstub.h to implement basic
 * behavior.  Output is a raw slip buffer. */
struct sbuf sbuf_from_usb; uint8_t sbuf_from_usb_buf[1024];
struct cbuf cbuf_to_usb;   uint8_t cbuf_to_usb_buf[1024];


/* The header has generic code, parameterized by platform dependent
 * functionality in terms of inline functions. */
#include "dht11.h"
struct dht11 dht11;

/* Instantiate the event handler. */
static void send(uint32_t event) {
    dht11_handle(&dht11, event);
}


/* A single general-purpose timer is used to provide two time-based
   mechanisms at different time scales:

   - The ability to measure microsecond-level time differences for
     decoding pulse width modulation.

   - A periodic interrupt to schedule millisecond-level events for
     output pulse width and overall timeout.

   Time differences are simplest to perform when the timer runs over
   the entire 16 bit range.  That requires div=0x10000 and gives a
   base clock of

   (/ 72000.0 #x10000) 1.1 kHz or 0.9us

   This fits perfectly tot the millisecond-level requirement for the
   interrupt, so set the prescaler to 1.
*/

const struct hw_periodic hw_tim_10us[] = {
//          rcc       irq            tim   div      pre
//-------------------------------------------------------
    [2] = { RCC_TIM2, NVIC_TIM2_IRQ, TIM2, 0x10000, 1 },
};
#define TIM 2
#define C_TIM hw_tim_10us[TIM]


/* Number of integral ticks per millisecond. */
#define ITICKS_PER_MS (72000.0/((double)0x10000))

/* Number of microseconds per fractional tick. */
#define US_PER_FTICK ((double)(1.0/72.0))



/* The current hardware timer value is used for fine scale
 * measurements. */
volatile uint16_t timer_frac_mark;
static inline void dht11_hw_time_zero(struct dht11 *s) {
    timer_frac_mark = hw_tim_counter(C_TIM.tim);
}
static inline uint32_t dht11_hw_time_elapsed_us(struct dht11 *s) {
    uint16_t fticks = hw_tim_counter(C_TIM.tim) - timer_frac_mark;
    uint32_t us = (((double)fticks) * US_PER_FTICK);
    // infof("us = %d\n", us);
    return us;
}


/* The integral number of 1.1kHz ticks is used for alarm events. */
volatile uint32_t timer_ticks;
volatile uint32_t timer_alarm_mark;
volatile uint32_t timer_alarm_enable;
void HW_TIM_ISR(TIM)(void) {
    hw_periodic_ack(C_TIM);
    uint32_t ticks = timer_ticks++;
    if (timer_alarm_enable && (timer_alarm_mark == ticks)) {
        timer_alarm_enable = 0;
        send(DHT11_EVENT_ALARM);
    }
}
static inline void dht11_hw_alarm_start_ms(struct dht11 *s, uint32_t ms) {
    uint32_t tticks = (uint32_t)(ITICKS_PER_MS*((double)ms));
    //infof("alarm_start: ms=%d, tticks=%d\n", ms, tticks);
    timer_alarm_enable = 1;
    timer_alarm_mark = timer_ticks + tticks;
}


/* Power control.  This can be on when not communicating.

   From observing the charge over a 470uS cap, the device seems to
   perform a measurement after communication is done for about 200 ms.
   That is when we turn on the power on the line.

 */
#define POWER GPIOB,14
void power_on(void) {
    hw_gpio_config(POWER, HW_GPIO_CONFIG_OPEN_DRAIN_2MHZ);
    hw_gpio_write(POWER, 0);
}
void power_off(void) {
    hw_gpio_config(POWER, HW_GPIO_CONFIG_OPEN_DRAIN_2MHZ);
    hw_gpio_write(POWER, 1);
}


/* EXTI and GPIO interfaces.  This uses B15 as one of the 5V tolerant
   inputs for parasitic power operation. */
const struct hw_exti c_exti = HW_EXTI_B15_B;
#define COMM  GPIOB,15

#define C_EXTI c_exti
volatile uint32_t count_exti;
void exti15_10_isr(void) {
    hw_exti_ack(C_EXTI);
    uint32_t event = hw_gpio_read(COMM) ?
        DHT11_EVENT_POSEDGE :
        DHT11_EVENT_NEGEDGE;
    send(event);
    count_exti++;
}
void exti_init(void) {
    dht11_hw_io_write(&dht11, 1);
    hw_exti_init(C_EXTI);
    hw_exti_arm(C_EXTI);
}


// Write a weak 1 and a strong 0.
static inline void dht11_hw_io_write(struct dht11 *s, int val) {
    hw_gpio_config(COMM, HW_GPIO_CONFIG_OPEN_DRAIN_2MHZ);
    hw_gpio_write(COMM, val);
}


/* Measurement result sink.  DHT11 and DHT22 use different encoding:
 * DHT11 only uses high byte for integral values, DHT22 uses hi:lo big
 * endian for decimal .1 increments. */
static inline void dht11_hw_response(struct dht11 *s, int ok, uint8_t *d) {

    /* UC has released line.  We can turn the power on again. */
    power_on();

    // FIXME: There is a (slipstub?) polling bug: messages are not
    // sent to the usb unless we also write something to the info log.
    if (1) { // verbose
        uint16_t rh = d[0]*256+d[1];
        uint16_t  t = d[2]*256+d[3];
        int disp = 3;
        if (disp & 2) {
            infof("dht22: %d %d %d\n", ok, rh, t);
        }
        if (disp & 1) {
            infof("dht11: %d %d %d\n", ok, d[0], d[2]);
        }
    }
    CBUF_WRITE(&cbuf_to_usb, {1, 1, ok, d[0], d[1], d[2], d[3]});
    /* FIXME: Resources can be freed here. */
}

/* This function receives complete SLIP packets from USB.
   Note that the pbuf contains the tag in the first 2 bytes. */
static void dispatch(struct slipstub *s, uint16_t tag, const struct pbuf *p) {
    // infof("handle tag %04x\n", tag);
    switch(tag) {
    case 0x101:
        // dht11 ! {send_packet,<<16#101:16>>}.
        // info ends up on usb, tagged with 0x101.
        // TAG_REPLY isn't really necessary. This will only have a
        // single process attached.
        // infof("dht_request()\n");
        power_off();
        dht11_request(&dht11);
        break;

#if 0 // example TAG_REPLY
    case TAG_PING:
        //infof("ping:%d\n",p->count-2);
        cbuf_write_slip_tagged(&slip_out, TAG_REPLY,
                               &p->buf[2], p->count-2);
        break;
#endif


    default:
        infof("bad tag\n");
    }
}

struct slipstub slipstub = {
    .slip_in   = &sbuf_from_usb.c,
    .packet_in = &sbuf_from_usb.p,
    .slip_out  = &cbuf_to_usb,
    .dispatch  = dispatch
};

#include "gdbstub_api.h"

const char config_product[];
void start(void) {
    /* Low level application init.  Note that this needs to be called
     * manually after loading to initialize memory. */
    hw_app_init();

    SBUF_INIT(sbuf_from_usb);
    CBUF_INIT(cbuf_to_usb);

    /* GPIO & EXTI */
    rcc_periph_clock_enable(RCC_GPIOA | RCC_AFIO);

    /* Idle line */
    exti_init();

    /* Use a single periodic timer to provide time base.  If the
     * application allows for it -- basically a power consumption
     * requirement because CPU will do more work -- this is almost
     * always simpler than messing with timer configurations
     * directly. */
    hw_periodic_init(C_TIM);

    infof("product: %s \n",&config_product[0]);
}

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "DHT11 interface board";
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;
const char config_version[]      CONFIG_DATA_SECTION = BUILD;
const char config_protocol[]     CONFIG_DATA_SECTION = "{driver,dht11_slip,slip}";

struct gdbstub_config config CONFIG_HEADER_SECTION = {
    .manufacturer    = config_manufacturer,
    .product         = config_product,
    .firmware        = config_firmware,
    .version         = config_version,
    .protocol        = config_protocol,
    .start           = start,
    .switch_protocol = slipstub_switch_protocol,
};
