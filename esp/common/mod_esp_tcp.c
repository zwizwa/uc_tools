#ifndef MOD_ESP_TCP
#define MOD_ESP_TCP

#include "bios.h"


// Simplified wrapper for TCP server
struct esp_tcp_conn;
typedef void (*esp_tcp_conn_fn)(struct esp_tcp_conn *);
struct esp_tcp_conn {
    int sock;
    esp_tcp_conn_fn handle;
    jmp_buf abort;
    uint16_t port;
};


// TCP client
void esp_tcp_connect(struct esp_tcp_conn *s, char *host_ip) {

    int addr_family = 0;
    int ip_protocol = 0;

    while (1) {
        struct sockaddr_in dest_addr;
        inet_pton(AF_INET, host_ip, &dest_addr.sin_addr);
        dest_addr.sin_family = AF_INET;
        dest_addr.sin_port = htons(s->port);
        addr_family = AF_INET;
        ip_protocol = IPPROTO_IP;

        int sock =  socket(addr_family, SOCK_STREAM, ip_protocol);
        if (sock < 0) {
            ESP_LOGE(TAG, "socket: errno %d", errno);
            break;
        }
        ESP_LOGI(TAG, "connecting to %s:%d", host_ip, s->port);

        int err = connect(sock, (struct sockaddr *)&dest_addr, sizeof(dest_addr));
        if (err != 0) {
            ESP_LOGE(TAG, "connect: errno %d", errno);
            break;
        }
        ESP_LOGI(TAG, "connected");

        // monitor_loop(sock);
        s->sock = sock;
        s->handle(s);

        if (sock != -1) {
            ESP_LOGE(TAG, "shutdown");
            shutdown(sock, 0);
            close(sock);
        }
    }
}

// TCP server
static void esp_tcp_listen_task(void *ctx) {

    struct esp_tcp_conn *s = ctx;

    char addr_str[128];
    int addr_family = AF_INET;
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
        dest_addr_ip4->sin_port = htons(s->port);
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

        ESP_LOGI(TAG, "listening on port %d", s->port);

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

        // monitor_loop(sock);
        s->sock = sock;
        s->handle(s);

        shutdown(sock, 0);
        close(sock);
    }

CLEAN_UP:
    close(listen_sock);
    vTaskDelete(NULL);
}

void esp_tcp_listen(struct esp_tcp_conn *s) {
    xTaskCreate(esp_tcp_listen_task, "esp_tcp_listen", 4096, s, 5, NULL);
}

#endif
