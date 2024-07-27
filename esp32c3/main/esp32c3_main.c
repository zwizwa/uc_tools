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

#include "esp_os.h"

static const char *TAG = "app";


/* uc_tools */

/* The GCC in it's default setup doesn't seem to support computed
   goto.  Not sure what is going on here.  A workaround is to use a
   blocking read instead, which is ok since we have an RTOS here. */
#define MONITOR_3IF_BLOCKING
#define TO_FLASH_3IF(s)   monitor_i3f_write_iram_byte(s, s->byte)
#define FROM_FLASH_3IF(s) s->byte = monitor_i3f_read_iram_byte(s)
struct monitor_3if;
void    monitor_i3f_write_iram_byte(struct monitor_3if *s, uint8_t byte);
uint8_t monitor_i3f_read_iram_byte(struct monitor_3if *s);

//#define MONITOR_3IF_LOG(s, ...) printf(__VA_ARGS__)
#include "../../mod_monitor_3if.c"




/* Hard-code it for now. */
#include "wifi.secret"
#ifndef WIFI_SSID
#error need WIFI_SSID
#endif
#ifndef WIFI_PASS
#error need WIFI_PASS
#endif
#ifndef HOST_IP
#error need HOST_IP
#endif

/* Config */

// Wifi
#define MAXIMUM_RETRY  5

// TCP server
#define KEEPALIVE_IDLE      5
#define KEEPALIVE_INTERVAL  5
#define KEEPALIVE_COUNT     3

#define PORT 12345


/* The idea is to use the monitor to load applets to SRAM for rapid
   edit-compile-run cycle exploration.  The existing build and flash
   operations work fine for production but are a bit too slow to my
   taste.  The idea is to expose functionality in a struct, and only
   reload the firmware when this functionality changes.  The code in
   SRAM could then be updated more frequently. */

void log_u32(uint32_t tag) {
    printf("log_u32: %08lx\n", tag);
}

const struct esp_os esp_os = {
    .malloc = malloc,
    .printf = printf,
    .log_u32 = log_u32,
};


/* Code buffer needs to be in IRAM. */

// FIXME: I'm just using 0x40098000 now, the top 32k of SRAM0 IRAM,
// which happens to be free.  This here doesn't seem to work: it just
// gives a DRAM address 0x3F......

// IRAM_BSS_ATTR uint32_t monitor_scratch[4 * 1024];


struct monitor_esp {
    /* Monitor state. */
    struct monitor_3if m;
    /* Pointer to struct with OS functionality is mapped into register
       space.  Currently this starts at register 9. */
    const struct esp_os *esp_os;  /*  9 */
    int sock;                     /* 10 */
    /* Misc state not mapped into 3if register space can be mapped here. */
    union {
        uint8_t u8[4];
        uint32_t u32;
    } iram_buf;
    jmp_buf abort;
    uint8_t out_buf[256];
    uint8_t ds_buf[256];
};

struct monitor_esp monitor_esp = { };

struct monitor_3if;
uint8_t monitor_3if_read_byte(struct monitor_3if *s) {
    struct monitor_esp *me = (void*)s;
    uint8_t byte;
    int rv = recv(me->sock, &byte, 1, 0);
    if (rv != 1) {
        ESP_LOGE(TAG, "recv: rv=%d", rv);
        longjmp(me->abort, 1);
    }
    return byte;
}
void monitor_3if_write_byte(struct monitor_3if *s, uint8_t byte) {
    struct monitor_esp *me = (void*)s;
    int rv = send(me->sock, &byte, 1, 0);
    if (rv != 1) {
        ESP_LOGE(TAG, "send: rv=%d", rv);
        longjmp(me->abort, 2);
    }
}
/* The IRAM can only be read or written as 32bit words, so cache reads
   and writes such that a normal byte sequence transfer will work.
   Use the Flash mode for this. */
void monitor_i3f_write_iram_byte(struct monitor_3if *s, uint8_t byte) {
    struct monitor_esp *me = (void*)s;
    uint32_t offset = ((uint32_t)s->flash) & 3;
    me->iram_buf.u8[offset] = byte;
    if (offset == 3) {
        /* Flush */
        uint32_t *u32 = (void*)(s->flash - 3);
        *u32 = me->iram_buf.u32;
    }
    s->flash++;
}
uint8_t monitor_i3f_read_iram_byte(struct monitor_3if *s) {
    struct monitor_esp *me = (void*)s;
    uint32_t offset = ((uint32_t)s->flash) & 3;
    if (offset == 0) {
        /* Cache */
        uint32_t *u32 = (void*)(s->flash );
        me->iram_buf.u32 = *u32;
    };
    s->flash++;
    return me->iram_buf.u8[offset];
}


void monitor_loop(int sock) {
    struct monitor_esp *me = &monitor_esp;
    monitor_3if_init(&me->m, me->ds_buf);
    me->sock = sock;
    me->esp_os = &esp_os;
    if (0 == setjmp(me->abort)) {
        monitor_3if_loop(&me->m);
    }
}

#if 0
// TCP client
void start_monitor(void)
{
    char host_ip[] = HOST_IP;
    int addr_family = 0;
    int ip_protocol = 0;

    while (1) {
        struct sockaddr_in dest_addr;
        inet_pton(AF_INET, host_ip, &dest_addr.sin_addr);
        dest_addr.sin_family = AF_INET;
        dest_addr.sin_port = htons(PORT);
        addr_family = AF_INET;
        ip_protocol = IPPROTO_IP;

        int sock =  socket(addr_family, SOCK_STREAM, ip_protocol);
        if (sock < 0) {
            ESP_LOGE(TAG, "socket: errno %d", errno);
            break;
        }
        ESP_LOGI(TAG, "connecting to %s:%d", host_ip, PORT);

        int err = connect(sock, (struct sockaddr *)&dest_addr, sizeof(dest_addr));
        if (err != 0) {
            ESP_LOGE(TAG, "connect: errno %d", errno);
            break;
        }
        ESP_LOGI(TAG, "connected");

        monitor_loop(sock);

        if (sock != -1) {
            ESP_LOGE(TAG, "shutdown");
            shutdown(sock, 0);
            close(sock);
        }
    }
}
#else
// TCP server
static void tcp_server_task(void *pvParameters)
{
    char addr_str[128];
    int addr_family = (int)pvParameters;
    int ip_protocol = 0;
    int keepAlive = 1;
    int keepIdle = KEEPALIVE_IDLE;
    int keepInterval = KEEPALIVE_INTERVAL;
    int keepCount = KEEPALIVE_COUNT;
    struct sockaddr_storage dest_addr;

    if (addr_family == AF_INET) {
        struct sockaddr_in *dest_addr_ip4 = (struct sockaddr_in *)&dest_addr;
        dest_addr_ip4->sin_addr.s_addr = htonl(INADDR_ANY);
        dest_addr_ip4->sin_family = AF_INET;
        dest_addr_ip4->sin_port = htons(PORT);
        ip_protocol = IPPROTO_IP;
    }

    int listen_sock = socket(addr_family, SOCK_STREAM, ip_protocol);
    if (listen_sock < 0) {
        ESP_LOGE(TAG, "socket: errno %d", errno);
        vTaskDelete(NULL);
        return;
    }
    int opt = 1;
    setsockopt(listen_sock, SOL_SOCKET, SO_REUSEADDR, &opt, sizeof(opt));

    int err = bind(listen_sock, (struct sockaddr *)&dest_addr, sizeof(dest_addr));
    if (err != 0) {
        ESP_LOGE(TAG, "bind: errno %d", errno);
        ESP_LOGE(TAG, "IPPROTO: %d", addr_family);
        goto CLEAN_UP;
    }

    err = listen(listen_sock, 1);
    if (err != 0) {
        ESP_LOGE(TAG, "listen: errno %d", errno);
        goto CLEAN_UP;
    }

    while (1) {

        ESP_LOGI(TAG, "listening on port %d", PORT);

        struct sockaddr_storage source_addr; // Large enough for both IPv4 or IPv6
        socklen_t addr_len = sizeof(source_addr);
        int sock = accept(listen_sock, (struct sockaddr *)&source_addr, &addr_len);
        if (sock < 0) {
            ESP_LOGE(TAG, "accept: errno %d", errno);
            break;
        }

        // Set tcp keepalive option
        setsockopt(sock, SOL_SOCKET, SO_KEEPALIVE, &keepAlive, sizeof(int));
        setsockopt(sock, IPPROTO_TCP, TCP_KEEPIDLE, &keepIdle, sizeof(int));
        setsockopt(sock, IPPROTO_TCP, TCP_KEEPINTVL, &keepInterval, sizeof(int));
        setsockopt(sock, IPPROTO_TCP, TCP_KEEPCNT, &keepCount, sizeof(int));
        // Convert ip address to string
        if (source_addr.ss_family == PF_INET) {
            inet_ntoa_r(((struct sockaddr_in *)&source_addr)->sin_addr,
                        addr_str, sizeof(addr_str) - 1);
        }
        ESP_LOGI(TAG, "accepted from %s", addr_str);

        monitor_loop(sock);

        shutdown(sock, 0);
        close(sock);
    }

CLEAN_UP:
    close(listen_sock);
    vTaskDelete(NULL);
}
void start_monitor(void) {
    xTaskCreate(tcp_server_task, "tcp_server", 4096, (void*)AF_INET, 5, NULL);
}
#endif







/* FreeRTOS event group to signal when we are connected*/
static EventGroupHandle_t s_wifi_event_group;

/* The event group allows multiple bits for each event, but we only care about two events:
 * - we are connected to the AP with an IP
 * - we failed to connect after the maximum amount of retries */
#define WIFI_CONNECTED_BIT BIT0
#define WIFI_FAIL_BIT      BIT1


static int s_retry_num = 0;


static void event_handler(void* arg, esp_event_base_t event_base,
                                int32_t event_id, void* event_data)
{
    if (event_base == WIFI_EVENT && event_id == WIFI_EVENT_STA_START) {
        esp_wifi_connect();
    } else if (event_base == WIFI_EVENT && event_id == WIFI_EVENT_STA_DISCONNECTED) {
        if (s_retry_num < MAXIMUM_RETRY) {
            esp_wifi_connect();
            s_retry_num++;
            ESP_LOGI(TAG, "retry to connect to the AP");
        } else {
            xEventGroupSetBits(s_wifi_event_group, WIFI_FAIL_BIT);
        }
        ESP_LOGI(TAG,"connect to the AP fail");
    } else if (event_base == IP_EVENT && event_id == IP_EVENT_STA_GOT_IP) {
        s_retry_num = 0;
        xEventGroupSetBits(s_wifi_event_group, WIFI_CONNECTED_BIT);
    }
}

void wifi_init_sta(void)
{
    s_wifi_event_group = xEventGroupCreate();

    ESP_ERROR_CHECK(esp_netif_init());

    ESP_ERROR_CHECK(esp_event_loop_create_default());
    esp_netif_create_default_wifi_sta();

    wifi_init_config_t cfg = WIFI_INIT_CONFIG_DEFAULT();
    ESP_ERROR_CHECK(esp_wifi_init(&cfg));

    esp_event_handler_instance_t instance_any_id;
    esp_event_handler_instance_t instance_got_ip;
    ESP_ERROR_CHECK(esp_event_handler_instance_register(WIFI_EVENT,
                                                        ESP_EVENT_ANY_ID,
                                                        &event_handler,
                                                        NULL,
                                                        &instance_any_id));
    ESP_ERROR_CHECK(esp_event_handler_instance_register(IP_EVENT,
                                                        IP_EVENT_STA_GOT_IP,
                                                        &event_handler,
                                                        NULL,
                                                        &instance_got_ip));

    wifi_config_t wifi_config = {
        .sta = {
            .ssid =     WIFI_SSID,
            .password = WIFI_PASS,
        },
    };
    ESP_ERROR_CHECK(esp_wifi_set_mode(WIFI_MODE_STA) );
    ESP_ERROR_CHECK(esp_wifi_set_config(WIFI_IF_STA, &wifi_config) );
    ESP_ERROR_CHECK(esp_wifi_start() );

    ESP_LOGI(TAG, "wifi_init_sta finished.");

    /* Waiting until either the connection is established (WIFI_CONNECTED_BIT) or connection failed for the maximum
     * number of re-tries (WIFI_FAIL_BIT). The bits are set by event_handler() (see above) */
    EventBits_t bits = xEventGroupWaitBits(s_wifi_event_group,
            WIFI_CONNECTED_BIT | WIFI_FAIL_BIT,
            pdFALSE,
            pdFALSE,
            portMAX_DELAY);

    /* xEventGroupWaitBits() returns the bits before the call returned, hence we can test which event actually
     * happened. */
    if (bits & WIFI_CONNECTED_BIT) {
    } else if (bits & WIFI_FAIL_BIT) {
        ESP_LOGI(TAG, "Failed to connect to SSID:%s, password:%s",
                 WIFI_SSID, WIFI_PASS);
    } else {
        ESP_LOGE(TAG, "UNEXPECTED EVENT");
    }
}

extern uint32_t _iram_end;
extern uint32_t _heap_start;
void meminfo(void) {
    printf("%p _iram_end\n", &_iram_end);
    printf("%p _heap_start\n", &_heap_start);
}

void app_main(void)
{

    //Initialize NVS
    esp_err_t ret = nvs_flash_init();
    if (ret == ESP_ERR_NVS_NO_FREE_PAGES || ret == ESP_ERR_NVS_NEW_VERSION_FOUND) {
      ESP_ERROR_CHECK(nvs_flash_erase());
      ret = nvs_flash_init();
    }
    ESP_ERROR_CHECK(ret);

    ESP_LOGI(TAG, "ESP_WIFI_MODE_STA");
    wifi_init_sta();

    meminfo();

    start_monitor();
}


