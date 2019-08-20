/* DHT11 interface

   Showcase for edge-triggered "parser".  Runs from interrupt using
   EXTI on both edges.  Time stamping is done using a free running
   timer.  Capture/compare seems overkill.

   TODO: Interrupt and timer framework are up, now create the bit
   event parser.

*/

#include "base.h"
#include "gdbstub_api.h"
#include <string.h>
#include "cbuf.h"

struct cbuf to_usb; uint8_t to_usb_buf[1024];

volatile uint32_t count = 0;
volatile uint32_t rollover = 0;

void poll(void) {
    static uint32_t track_count = 0;
    static uint32_t track_rollover = 0;
    if (track_count != count) {
        uint8_t msg[] = {0,0,0,1,'P'};
        cbuf_write(&to_usb, msg, sizeof(msg));
        track_count++;
    }
    while (track_rollover != rollover) {
        uint8_t msg[] = {0,0,0,1,'T'};
        cbuf_write(&to_usb, msg, sizeof(msg));
        track_rollover++;
    }
}


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


const struct hw_exti c_exti = HW_EXTI_A0_B;
#define C_EXTI c_exti

void exti0_isr(void) {
    hw_exti_ack(C_EXTI);
    count++;
}

//          rcc       irq            tim   div      pre
//-------------------------------------------------------
const struct hw_periodic hw_tim_10us[] = {
    [2] = { RCC_TIM2, NVIC_TIM2_IRQ, TIM2, 0x10000, 720 },
};

#define TIM 2
#define C_TIM hw_tim_10us[TIM]


void HW_TIM_ISR(TIM)(void) {
    /* 655.360ms period.  This isn't used for anything, but config has
     * interrupt enabled. */
    hw_periodic_ack(C_TIM);
    rollover++;
}


const char config_product[];
void start(void) {
    /* Low level application init.  Note that this needs to be called
     * manually after loading to initialize memory. */
    hw_app_init();
    CBUF_INIT(to_usb);
    uint8_t msg[] = {0,0,0,1,123};
    cbuf_write(&to_usb, msg, sizeof(msg));

    /* Timers are 16 bits with 16 bit prescaling value. We want about
     * 10uS resolution for a total cycle of 655.360 ms. */
    hw_periodic_init(C_TIM);

    /* GPIO & EXTI */
    rcc_periph_clock_enable(RCC_GPIOA | RCC_AFIO);
    hw_gpio_high(GPIOA,0); // pull up
    hw_gpio_config(GPIOA,0,HW_GPIO_CONFIG_INPUT_PULL);
    hw_exti_init(C_EXTI);
    hw_exti_arm(C_EXTI);

    _service.add(poll);

    infof("product: %s\n",&config_product[0]);

}

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "DHT11 board";
const char config_serial[]       CONFIG_DATA_SECTION = "123";
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;
const char config_version[]      CONFIG_DATA_SECTION = BUILD;
const char config_protocol[]     CONFIG_DATA_SECTION = "{packet,4}";

struct gdbstub_config config CONFIG_HEADER_SECTION = {
    .manufacturer    = config_manufacturer,
    .product         = config_product,
    .serial          = config_serial,
    .firmware        = config_firmware,
    .version         = config_version,
    //.protocol        = config_protocol,
    .start           = start,
    .switch_protocol = switch_protocol,
};






