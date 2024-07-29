#include "generic.h"
#include "gdbstub.h"
#include "gdbstub_api.h"

#if defined(STM32F1)
#include "hw_stm32f103.h"
#elif defined(STM32F4)
#include "hw_stm32f407.h"
#else
#error UNKNOWN_HW
#endif

extern struct gdbstub_service service;

// cdcacm_desc.c
void cdcacm_set_config_with_callbacks(usbd_device *, void*, void*);
usbd_device *cdcacm_init(void *set_config, const char * const *usb_strings);

/* CDCACM */
usbd_device *usb_serial;
static const char * usb_strings[3];

/* Serial data I/O */
uint8_t rx_buf[64];
uint8_t tx_buf[64];
uint32_t tx_buf_sema;

/* Send len bytes from tx_buf */
static void data_tx(uint32_t len) {
    tx_buf_sema++;
    usbd_ep_write_packet(usb_serial, 0x82, tx_buf, len);
}
/* Poll output state machine. */
static void poll_data_tx(void) {
    if (tx_buf_sema > 0) return; // busy
    uint32_t len = io->read(tx_buf, sizeof(tx_buf));
    if (len) data_tx(len);
}
/* USB transmission complete. */
static void data_tx_cb(usbd_device *usbd_dev, uint8_t ep) {
    if (usbd_dev != usb_serial) return;
    tx_buf_sema--;
    /* Immediately check if there's more. */
    poll_data_tx();
}
/* Receive bytes into rx_buf, return number. */
static uint32_t data_rx(void) {
    return usbd_ep_read_packet(usb_serial, 0x01, rx_buf, sizeof(rx_buf));
}
/* USB has data available.  Push it to state machine + poll TX. */
static void data_rx_cb(usbd_device *usbd_dev, uint8_t ep) {
    if (usbd_dev != usb_serial) return;
    uint32_t len = data_rx();
    io->write(rx_buf, len);
    /* Immediately check if there's anything available. */
    poll_data_tx();
}

void bootloader_io_reset(void); // FIXME: is this necessary?
static void cdcacm_set_config(usbd_device *usbd_dev, uint16_t wValue) {
    (void)wValue;
    cdcacm_set_config_with_callbacks(usbd_dev, data_rx_cb, data_tx_cb);
    usbd_register_reset_callback(usbd_dev, bootloader_io_reset);
}

char serial_hex[25];

void hw_bootloader_usb_init(void) {

    // setup clock in bl_*.c
    //rcc_clock_setup_in_hse_8mhz_out_72mhz();  // FIXME: was
    //rcc_clock_setup_in_hse_12mhz_out_72mhz();  // FIXME: was

    for (uint32_t i=0; i<12; i++) {
        // FIXME: share this code with smpacket.h
        static const uint8_t digit[] = "0123456789abcdef";
        uint8_t b = ((uint8_t*)0x1FFFF7E8)[i];
        uint8_t hi = digit[(b >> 4) & 0x0f];
        uint8_t lo = digit[(b >> 0) & 0x0f];
        serial_hex[2*i] = hi;
        serial_hex[2*i+1] = lo;
    }

    rcc_periph_clock_enable(RCC_GPIOA);
#ifdef RCC_AFIO
    rcc_periph_clock_enable(RCC_AFIO);
#endif

    usb_strings[0] = flash_string(_config.manufacturer, "Zwizwa");
    usb_strings[1] = flash_string(_config.product,      "Bootloader");
    usb_strings[2] = flash_string(_config.serial,       serial_hex);

    usb_serial = cdcacm_init(cdcacm_set_config, usb_strings);
}

void hw_bootloader_usb_poll(void) {
    usbd_poll(usb_serial);
    poll_data_tx(); // for async data
}
