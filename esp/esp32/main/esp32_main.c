/* Exploratory ESP32 application.

   Documentation:
   https://github.com/espressif/esp-idf/blob/master/examples/wifi/getting_started/station/main/station_example_main.c
   https://github.com/espressif/esp-idf/blob/master/examples/get-started/hello_world/main/hello_world_main.c
   https://github.com/espressif/esp-idf/tree/master/examples/protocols/sockets/tcp_client
   https://github.com/espressif/esp-idf/tree/master/examples/protocols/sockets/tcp_server

   Questions:
   - Why is nvs needed?
   - How is this so large? 760k? Is that typical?
*/

// ssh mimas "cd /i/exo/uc_tools/esp32/build ; /nix/store/5lbxsj5mnz95rq5hkq7ixxb1cg96k07g-ninja-1.11.1/bin/ninja"
// ./tether_bl.dynamic.host.elf 192.168.0.122 load 0x40098000 /i/exo/uc_tools/esp32/plugin/test.bin
// ./tether_bl.dynamic.host.elf 192.168.0.122 run_ram 0x40098000


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


#include "../../common/mod_esp_3if.c"
#include "../../common/mod_esp_wifi.c"
//#include "mod_esp_swd.c"


uint8_t dram_buf[32*1024];

void app_main(void)
{

    wifi_start();

    // Memory info for host side plugin linker.

    // Use a dedicated buffer
    meminfo.ram_addr = (uint32_t)dram_buf;
    meminfo.ram_len = sizeof(dram_buf);

    // All free instrunction sram, otherwise unused.
    meminfo.flash_addr = (uint32_t)&_iram_end;
    meminfo.flash_len = 0x400A0000 - (uint32_t)&_iram_end;

    start_monitor();
}


