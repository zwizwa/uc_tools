#ifndef MOD_ESP_3IF
#define MOD_ESP_3IF


/* The idea is to use the monitor to load applets to SRAM for rapid
   edit-compile-run cycle exploration.  The existing build and flash
   operations work fine for production but are a bit too slow to my
   taste.  The idea is to expose functionality in a struct, and only
   reload the firmware when this functionality changes.  The code in
   SRAM could then be updated more frequently. */


#include "esp_ota_ops.h"

#include "../../bios.h"




/* The GCC in it's default setup doesn't seem to support computed
   goto.  Not sure what is going on here.  A workaround is to use a
   blocking read instead, which is ok since we have an RTOS here. */
#if 1
#define MONITOR_3IF_BLOCKING
/* f is for fussy */
#define TO_FLASH_3IF(s)   monitor_i3f_write_fussy_byte(s, s->byte)
#define FROM_FLASH_3IF(s) s->byte = monitor_i3f_read_fussy_byte(s)
struct monitor_3if;
void    monitor_i3f_write_fussy_byte(struct monitor_3if *s, uint8_t byte);
uint8_t monitor_i3f_read_fussy_byte(struct monitor_3if *s);
//#define MONITOR_3IF_LOG(s, ...) printf(__VA_ARGS__)
#else
// Make it work in push mode again.  Best to keep all code the same.
// The previous issue was just due to the dangling-pointer warning.
#endif
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Wdangling-pointer"
#include "../../mod_monitor_3if.c"
#pragma GCC diagnostic pop

#include "xtensa/core-macros.h"
uint32_t esp_cycle_counter(void) {
    /* Note that this has one per CPU, so make sure that different
       calls are happening on the same CPU. */
    return XTHAL_GET_CCOUNT();
}

const struct iot_ota iot_ota = {
    .get_sha256 = esp_app_get_elf_sha256,
    .begin = esp_ota_begin,
    .write = esp_ota_write,
    .end = esp_ota_end,
};

const struct bios bios = {
    .malloc = malloc,
    .printf = printf,
    .reboot = esp_restart,
    .iot_ota = &iot_ota,
    .cycle_counter = esp_cycle_counter,
};

struct monitor_esp {
    /* Monitor state. */
    union {
        struct monitor_3if m;
        uint32_t reg[16];
    } state;
    /* Plugin code is provided with BIOS code pointers. */
    const struct bios *bios;  /* 16 */
    /* To implement 3if extensions that will interact with the socket. */
    int sock;                         /* 17 */
    /* Misc state not mapped into 3if register space can be mapped here. */
    union {
        uint8_t u8[4];
        uint32_t u32;
    } fussy_buf;
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
        if (rv == 0) {
            ESP_LOGI(TAG, "closed");
        }
        else {
            ESP_LOGE(TAG, "recv: rv=%d", rv);
        }
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
void monitor_i3f_write_fussy_byte(struct monitor_3if *s, uint8_t byte) {
    struct monitor_esp *me = (void*)s;
    uint32_t offset = ((uint32_t)s->flash) & 3;
    me->fussy_buf.u8[offset] = byte;
    if (offset == 3) {
        /* Flush */
        uint32_t *u32 = (void*)(s->flash - 3);
        *u32 = me->fussy_buf.u32;
    }
    s->flash++;
}

extern uint32_t _iram_end;
struct monitor_3if_meminfo meminfo;

uint8_t monitor_i3f_read_fussy_byte(struct monitor_3if *s) {
    struct monitor_esp *me = (void*)s;

    /* Memory-mapping is done here since we're already special-casing
       the mechanism. */
    if (s->flash == 0) {
        /* 0x00000000 maps to the 3if state.  This can be used to read
           out the memory layout info. */
        s->flash = (uint8_t *)(&meminfo);
    }

    uint32_t offset = ((uint32_t)s->flash) & 3;
    if (offset == 0) {
        /* Cache */
        uint32_t *u32 = (void*)(s->flash );
        me->fussy_buf.u32 = *u32;
    };
    s->flash++;
    return me->fussy_buf.u8[offset];
}


void monitor_loop(int sock) {
    struct monitor_esp *me = &monitor_esp;
    monitor_3if_init(&me->state.m, me->ds_buf);
    me->sock = sock;
    me->bios = &bios;

    if (0 == setjmp(me->abort)) {
        monitor_3if_loop(&me->state.m);
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
        dest_addr.sin_port = htons(MONITOR_PORT);
        addr_family = AF_INET;
        ip_protocol = IPPROTO_IP;

        int sock =  socket(addr_family, SOCK_STREAM, ip_protocol);
        if (sock < 0) {
            ESP_LOGE(TAG, "socket: errno %d", errno);
            break;
        }
        ESP_LOGI(TAG, "connecting to %s:%d", host_ip, MONITOR_PORT);

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
        dest_addr_ip4->sin_port = htons(MONITOR_PORT);
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

        ESP_LOGI(TAG, "listening on port %d", MONITOR_PORT);

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





#endif
