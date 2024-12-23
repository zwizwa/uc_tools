/* See more comments in plain esp32 files.

   What is different here from plain esp32 is the memory layout for
   the 3if monitor.  See esp32s3/build/esp-idf/esp_system/ld/memory.ld
*/



#include <string.h>
#include <stdio.h>
#include <inttypes.h>
#include <unistd.h>
#include <sys/socket.h>
#include <errno.h>
#include <netdb.h>            // struct addrinfo
#include <arpa/inet.h>
#include <setjmp.h>
#include "freertos/FreeRTOS.h"
#include "freertos/task.h"
#include "freertos/event_groups.h"
#include "sdkconfig.h"
#include "esp_chip_info.h"
#include "esp_flash.h"
#include "esp_system.h"
#include "esp_wifi.h"
#include "esp_event.h"
#include "esp_log.h"
#include "esp_netif.h"
#include "nvs_flash.h"
#include "lwip/err.h"
#include "lwip/sys.h"
#include "xtensa/core-macros.h"

#include "driver/gpio.h"
#include "neopixel.h"

#include "esp_dmx.h"
#include "driver/periph_ctrl.h" // FIXME: Header is deprecated, fix in esp_dmx


/* Config */

// FIXME: Handle log tags better.  ATM all mod_*.c expect this to be define.
#define TAG __func__

// Wifi
#define MAXIMUM_RETRY  5

// TCP server
#define KEEPALIVE_IDLE      5
#define KEEPALIVE_INTERVAL  5

#define KEEPALIVE_COUNT     3

#define MONITOR_PORT 12345


/* uc_tools modules */
#define infof printf
#define info_puts puts


/* uc_tools modules */
#define infof printf
#define info_puts puts


uint8_t dram_buf[32*1024];

#if 1
__attribute__((section(".iram.bss")))
uint8_t iram_buf[32*1024];
#define iram_buf_size sizeof(iram_buf)
#else
/* From memory.ld: Startup code uses the IRAM from 0x403B9000 to
   0x403E0000, which is not available for static memory, but can only
   be used after app starts. */
#define iram_buf       0x403B9000
#define iram_buf_endx  0x403E0000
#define iram_buf_size (iram_buf_endx - iram_buf)
#endif


#include "../../common/mod_esp_3if.c"
#include "../../common/mod_esp_wifi.c"
#include "../../common/mod_esp_acm.c"

tNeopixelContext neopixel;
void neopixel_start(void) {
    neopixel = neopixel_Init(100, GPIO_NUM_48);
    // FIXME: assert
}


void app_main(void)
{


#if 0 // NEOPIXEL
    neopixel_start();
    tNeopixel pixel[] = {
        { 0, NP_RGB(50, 0,  0) }, /* red */
        { 0, NP_RGB(0,  50, 0) }, /* green */
        { 0, NP_RGB(0,  0, 50) }, /* blue */
        { 0, NP_RGB(0,  0,  0) }, /* off */
    };
    for (;;) {
        for(int i = 0; i < ARRAY_SIZE(pixel); ++i) {
            ESP_LOGI(TAG, "%08lx", pixel[i].rgb);
            neopixel_SetPixel(neopixel, &pixel[i], 1);
            vTaskDelay(pdMS_TO_TICKS(200));
        }
    }
#endif

#if 0 // DMX

    /* Some initial questions:
       - How does it pick a UART device?
     */

    /* I've commented out these calls in the esp_dmx code to fix an
       esp idf regression (5.1 to 5.3, missing .module field).  So
       call them manually. */

    // esp_dmx expects uart_signal_conn_t to have a .module field
    //   periph_module_enable(uart_periph_signal[dmx_num].module);
    //   periph_module_reset(uart_periph_signal[dmx_num].module);
    //
    // see here for 5.1:
    // https://github.com/espressif/esp-idf/blob/release/v5.1/components/soc/include/soc/uart_periph.h
    //
    // this is implemented here:
    // https://github.com/espressif/esp-idf/blob/release/v5.1/components/soc/esp32s3/uart_periph.c
    //
    // the following code does it manually:
#if 1
    const periph_module_t module = PERIPH_UART1_MODULE;
    periph_module_enable(module);
    periph_module_reset(module);
#endif

    const dmx_port_t dmx_num = DMX_NUM_1;

    // First, use the default DMX configuration...
    dmx_config_t config = DMX_CONFIG_DEFAULT;

    // ...declare the driver's DMX personalities...
    const int personality_count = 1;
    dmx_personality_t personalities[] = {
        {1, "Default Personality"}
    };

    // ...install the DMX driver...
    dmx_driver_install(dmx_num, &config, personalities, personality_count);

    // ...and then set the communication pins!

    // How to pick? See ESP32S3 TRM, 6.12 IO Mux Function list and
    // pick defaults for U1TXD, U1RXD.  Then use adjacent unused pin.
    const int tx_pin  = 17;
    const int rx_pin  = 18;
    const int rts_pin = 16;
    dmx_set_pin(dmx_num, tx_pin, rx_pin, rts_pin);

    int dmx_count = 0;
    while (true) {
        ESP_LOGI(TAG, "dmx %d", dmx_count++);

        uint8_t data[DMX_PACKET_SIZE] = {0};

        // Write to the packet and send it.
        dmx_write(dmx_num, data, DMX_PACKET_SIZE);
        dmx_send(dmx_num);

        // Do work here...

        // Block until the packet is finished sending.
        dmx_wait_sent(dmx_num, DMX_TIMEOUT_TICK);
    }

#endif


    acm_bridge_start(&node_bridge);
    // acm_bridge_start(NULL);

    wifi_start();

#if 1

    // Memory info for host side plugin linker.

    // Use a dedicated buffer
    meminfo.ram_addr = (uint32_t)dram_buf;
    meminfo.ram_len = sizeof(dram_buf);

    // Use a dedicated buffer
    meminfo.flash_addr = (uint32_t)iram_buf;
    meminfo.flash_len = iram_buf_size;

    start_monitor();
#endif
}


