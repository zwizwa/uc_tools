/* Macro-configurable module for DHT11 support.

   Apart from libraries, the "module system" is just a convention,
   where .c files with a mod_ prefix are supposed to be included from
   main.c while depending on some configuration macros to be defined.

   Is this good practice?  I don't know.  Doing it at the C level
   requires a lot more work, so just accept that instantiation is more
   important than C-level abstraction.
*/

#ifndef MOD_DHT11_C
#define MOD_DHT11_C

/* The header has generic code, parameterized by platform dependent
 * functionality in terms of inline functions. */
#include "dht11.h"
struct dht11 dht11;

/* This module relies on a particular time base. */
CT_ASSERT(timebase_div, TIMEBASE_DIV == 0x10000);
#define TIMEBASE_ALARM() dht11_handle(&dht11, DHT11_EVENT_ALARM)
#include "mod_timebase.c"

/* Link some of the dht11.h hal hooks. */
static inline void dht11_hw_time_zero(struct dht11 *s) {
    timebase_set_mark();
}
static inline uint32_t dht11_hw_time_elapsed_us(struct dht11 *s) {
    return timebase_elapsed_us();
}
static inline void dht11_hw_alarm_start_ms(struct dht11 *s, uint32_t ms) {
    timebase_alarm_start_ms(ms);
}

/* Power control.  This can be on when not communicating.

   From observing the charge over a 470uS cap, the device seems to
   perform a measurement after communication is done for about 200 ms.
   That is when we turn on the power on the line.

 */
#ifndef DHT11_POWER
#error  DHT11_POWER
#endif

static inline void dht11_hw_power_on(struct dht11 *s) {
    hw_gpio_config(DHT11_POWER, HW_GPIO_CONFIG_OPEN_DRAIN_2MHZ);
    hw_gpio_write(DHT11_POWER, 0);
}
void dht11_hw_power_off(struct dht11 *s) {
    hw_gpio_config(DHT11_POWER, HW_GPIO_CONFIG_OPEN_DRAIN_2MHZ);
    hw_gpio_write(DHT11_POWER, 1);
}


/* EXTI and GPIO interfaces.  This uses B15 as one of the 5V tolerant
   inputs for parasitic power operation. */

/* FIXME: This is difficult to abstract due to shared interrupts. */

const struct hw_exti c_exti = HW_EXTI_B15_B;

#ifndef DHT11_COMM
#error  DHT11_COMM
#endif

#define C_EXTI c_exti
volatile uint32_t count_exti;
void exti15_10_isr(void) {
    hw_exti_ack(C_EXTI);
    uint32_t event = hw_gpio_read(DHT11_COMM) ?
        DHT11_EVENT_POSEDGE :
        DHT11_EVENT_NEGEDGE;
    dht11_handle(&dht11, event);
    count_exti++;
}
void exti_init(void) {
    dht11_hw_io_write(&dht11, 1);
    hw_exti_init(C_EXTI);
    hw_exti_arm(C_EXTI);
}


// Write a weak 1 and a strong 0.
static inline void dht11_hw_io_write(struct dht11 *s, int val) {
    hw_gpio_config(DHT11_COMM, HW_GPIO_CONFIG_OPEN_DRAIN_2MHZ);
    hw_gpio_write(DHT11_COMM, val);
}

/* Measurement result sink.  DHT11 and DHT22 use different encoding:
 * DHT11 only uses high byte for integral values, DHT22 uses hi:lo big
 * endian for decimal .1 increments. */
static inline void dht11_hw_response(struct dht11 *s, int ok, uint8_t *d) {

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



#endif
