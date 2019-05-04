// Wave form generator.
// See etf_test.c for a simpler ETF protocol example.

#include "sm_etf.h"
#include "base.h"
#include "gdbstub_api.h"
#include <string.h>

uint32_t dispatch(uint8_t type,
                  uint8_t *buf, uint32_t buf_len,
                  int32_t *tag, uint32_t tag_len) {
    infof("wavegen:(%d,%d)", type, buf_len);
    for(int i=0; i<=tag_len; i++) { infof(",%d", tag[i]); }
    infof("\n");
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
        infof("status=%x\n",status);
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

static void poll(void) {
    // nvic_enable_irq(NVIC_USB_LP_CAN_RX0_IRQ); // prepare wakeup events
    // __asm volatile( "wfi" );
    // __asm volatile( "wfi" ); // Doesn't work for USB in polling mode
    //__asm volatile( "wfe" );
    //infof("wfe\n");
    //nvic_clear_pending_irq(NVIC_USB_LP_CAN_RX0_IRQ);
    // Peripheral IRQ bits should be cleared in the poll loop.
}
void usb_lp_can_rx0_isr(void) {
    infof("rx\n");
    //nvic_disable_irq(NVIC_USB_LP_CAN_RX0_IRQ);
    //nvic_clear_pending_irq(NVIC_USB_LP_CAN_RX0_IRQ);
}
void usb_hp_can_tx_isr(void) {
    infof("tx\n");
}
void usb_wakeup_isr(void){
    infof("w\n");
}


/* STARTUP */
void switch_protocol(const uint8_t *buf, uint32_t size) {
    *_service.io = (struct gdbstub_io *)(&etf_io);
    (*_service.io)->write(buf, size);
}
void start(void) {
    hw_app_init();
    rcc_periph_clock_enable(RCC_GPIOA | RCC_AFIO);
    sm_etf_reset();

    /* To support WFE, enable the interrupt in the peripheral control
       register but not in the NVIC, and enable the SEVONPEND bit in
       the Cortex ® -M3 System Control register. */
    //SCB_SCR |= SCB_SCR_SEVEONPEND;

    //__asm volatile("cpsid i"); // make sure we never get interrupts
    //nvic_enable_irq(NVIC_USB_LP_CAN_RX0_IRQ); // prepare wakeup events
    //nvic_enable_irq(NVIC_USB_HP_CAN_TX_IRQ);
    //nvic_enable_irq(NVIC_USB_WAKEUP_IRQ);

    _service.add(poll);
}
void stop(void) {
    hw_app_stop();
    _service.reset();
}
int main(void) { start(); }

const char config_manufacturer[] CONFIG_DATA_SECTION = "Zwizwa";
const char config_product[]      CONFIG_DATA_SECTION = "Waveform Generator";
const char config_serial[]       CONFIG_DATA_SECTION = "000000000000000000000000";
const char config_firmware[]     CONFIG_DATA_SECTION = FIRMWARE;
const char config_version[]      CONFIG_DATA_SECTION = BUILD;

struct gdbstub_config config CONFIG_HEADER_SECTION = {
    .manufacturer    = config_manufacturer,
    .product         = config_product,
    .serial          = config_serial,
    .firmware        = config_firmware,
    .version         = config_version,
    .start           = start,
    .stop            = stop,
    .switch_protocol = switch_protocol,
};




