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

static const char *TAG = "app";


/* Config */

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

/* FIXME: This causes Guru Meditation Error: Core / panic'ed (Cache
   disabled but cached memory region accessed). */

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




void app_main(void)
{

    acm_start();

    wifi_start();

    // Memory info for host side plugin linker.

    // Use a dedicated buffer
    meminfo.ram_addr = (uint32_t)dram_buf;
    meminfo.ram_len = sizeof(dram_buf);

    // Use a dedicated buffer
    meminfo.flash_addr = (uint32_t)iram_buf;
    meminfo.flash_len = iram_buf_size;

    start_monitor();
}


