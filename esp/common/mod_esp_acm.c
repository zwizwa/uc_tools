#ifndef MOD_CSP_ACM
#define MOD_CSP_ACM

/* Bridge a USB TTY microcontroller to TCP.  To keep the ACM stream
   and TCP stream in sync we just boot the microcontroller on TCP
   connect and put it in reset on disconnect.  Crude but simple. */

/* FIXME:

   - It is set up now so that it works on the first TCP connection and
     the first ACM connection, but later this needs to be fleshed out
     with semaphores to keep the two sides in sync (= one TCP
     connection is one microcontroller boot cycle).


*/

#include "usb/usb_host.h"
#include "usb/cdc_acm_host.h"

// https://github.com/espressif/esp-idf/tree/f420609c332fbd2d2f7f188c6579d046c9560e42/examples/peripherals/usb/host/cdc/cdc_acm_host
#define USB_HOST_PRIORITY   (20)
#define USB_DEVICE_VID      (0x0483)
#define USB_DEVICE_PID      (0x5740)
#define TX_TIMEOUT_MS       (3000)


struct acm_bridge {
    struct esp_tcp_conn tcp_conn;
    cdc_acm_dev_hdl_t cdc_dev;
};



static SemaphoreHandle_t device_disconnected_sem;


#undef TAG
#define TAG __func__

static bool acm_bridge_handle_rx(const uint8_t *data, size_t data_len, void *ctx) {

    //ESP_LOGI(TAG, "Data received");
    //ESP_LOG_BUFFER_HEXDUMP(TAG, data, data_len, ESP_LOG_INFO);

    struct acm_bridge *s = ctx;
    int sock = s->tcp_conn.sock;
    if (sock == -1) {
        ESP_LOGI(TAG, "dropping %d bytes", data_len);
    }
    else {
        // ESP_LOGI(TAG, "forwarding %d bytes", data_len);
        send(sock, data, data_len, 0);
    }
    return true;
}
static void acm_bridge_handle_event(const cdc_acm_host_dev_event_data_t *event, void *ctx) {
    struct acm_bridge *s = ctx;
    (void)s;
    switch (event->type) {
    case CDC_ACM_HOST_ERROR:
        ESP_LOGE(TAG, "CDC-ACM error has occurred, err_no = %i", event->data.error);
        break;
    case CDC_ACM_HOST_DEVICE_DISCONNECTED:
        ESP_LOGI(TAG, "Device suddenly disconnected");
        ESP_ERROR_CHECK(cdc_acm_host_close(event->data.cdc_hdl));
        xSemaphoreGive(device_disconnected_sem);
        break;
    case CDC_ACM_HOST_SERIAL_STATE:
        ESP_LOGI(TAG, "Serial state notif 0x%04X", event->data.serial_state.val);
        break;
    case CDC_ACM_HOST_NETWORK_CONNECTION:
    default:
        ESP_LOGW(TAG, "Unsupported CDC event: %i", event->type);
        break;
    }
}
static void acm_bridge_usb_lib_task(void *ctx) {
    struct acm_bridge *s = ctx;
    (void)s;
    while (1) {
        // Start handling system events
        uint32_t event_flags;
        usb_host_lib_handle_events(portMAX_DELAY, &event_flags);
        if (event_flags & USB_HOST_LIB_EVENT_FLAGS_NO_CLIENTS) {
            ESP_ERROR_CHECK(usb_host_device_free_all());
        }
        if (event_flags & USB_HOST_LIB_EVENT_FLAGS_ALL_FREE) {
            ESP_LOGI(TAG, "USB: All devices freed");
            // Continue handling USB events to allow device reconnection
        }
    }
}
static void acm_bridge_acm_task(void *ctx) {
    struct acm_bridge *s = ctx;

    const cdc_acm_host_device_config_t dev_config = {
        .connection_timeout_ms = TX_TIMEOUT_MS,
        .out_buffer_size = 1024,
        .in_buffer_size = 1024,
        .user_arg = s,
        .event_cb = acm_bridge_handle_event,
        .data_cb = acm_bridge_handle_rx
    };

    while (1) {
        s->cdc_dev = NULL;

        // Open USB device from tusb_serial_device example example. Either single or dual port configuration.
        ESP_LOGI(TAG, "Opening CDC ACM device 0x%04X:0x%04X...", USB_DEVICE_VID, USB_DEVICE_PID);
        uint8_t interface_idx = 0;
        // uint8_t interface_idx = 1;
        esp_err_t err = cdc_acm_host_open(
            USB_DEVICE_VID, USB_DEVICE_PID, interface_idx, &dev_config, &s->cdc_dev);
        if (ESP_OK != err) {
            ESP_LOGI(TAG, "Failed to open device");
            continue;
        }
        ESP_LOGI(TAG, "Opened CDC ACM device 0x%04X:0x%04X...", USB_DEVICE_VID, USB_DEVICE_PID);

        // Wait for semaphore set by acm_handle_event() CDC_ACM_HOST_DEVICE_DISCONNECTED
        xSemaphoreTake(device_disconnected_sem, portMAX_DELAY);
        ESP_LOGI(TAG, "Disconnected CDC ACM device 0x%04X:0x%04X...", USB_DEVICE_VID, USB_DEVICE_PID);

    }
}



uint8_t node_read_byte(struct esp_tcp_conn *s) {
    uint8_t byte;
    int rv = recv(s->sock, &byte, 1, 0);
    if (rv != 1) {
        if (rv == 0) {
            ESP_LOGI(TAG, "closed");
        }
        else {
            ESP_LOGE(TAG, "recv: rv=%d", rv);
        }
        longjmp(s->abort, 1);
    }
    return byte;
}


void reset_peripheral(void) {
    gpio_num_t gpio = GPIO_NUM_4;

    // Drive reset pin to GND.
    gpio_set_level(gpio, 0);
    gpio_set_direction(gpio, GPIO_MODE_OUTPUT);

    vTaskDelay(pdMS_TO_TICKS(10));

    // Release the pin again, leaving it under control of its pullup
    // circuit.
    gpio_set_direction(gpio, GPIO_MODE_INPUT);

}

void node_loop(struct esp_tcp_conn *conn) {
    struct acm_bridge *s = (void*)conn;

    /* Once the TCP connection is up, activate the device.

       Goal: make sure nothing gets missed, because the protocol uses
       size prefixes and cannot resynchronize.

       FIXME: Later this should be done by bringing it out if reset
       and letting it re-enumerate.  But for now this works: device
       will only start sending if it receives a packet.  We use a ping
       packet for that.  Reason is not clear (bootloader->app
       transition? or some other logging flag?)
    */

    uint8_t hello_msg[] = {0,0,0,2, 0xFF,0xFC};
    ESP_ERROR_CHECK(
        cdc_acm_host_data_tx_blocking(
            s->cdc_dev,
            hello_msg, sizeof(hello_msg),
            TX_TIMEOUT_MS));

    if (0 == setjmp(conn->abort)) {
      again:
#if 0
        // vTaskDelay(pdMS_TO_TICKS(1000));
        uint8_t byte = node_read_byte(conn);
        ESP_LOGI(TAG, "byte = %d", byte);
#else
        uint8_t buf[1024]; // What's a good size?  This can hold the largest packet.
        int rv = recv(conn->sock, buf, sizeof(buf), 0);
        if (rv > 0) {
            // ok
            ESP_ERROR_CHECK(
                cdc_acm_host_data_tx_blocking(
                    s->cdc_dev,
                    buf, rv,
                    TX_TIMEOUT_MS));
        }
        else {
            // error, e.g. disconnect
            ESP_LOGE(TAG, "recv = %d", rv);

            // reset the device here so it will be MOST LIKELY
            // connected again when TCP reconnects.  FIXME: this is a
            // race condition, but probably not one that easily
            // triggers (tcp assumes device pointer to be operational
            // right after connect but there is nothing that actually
            // guarantees that).
            reset_peripheral();

            return;
        }
#endif
        goto again;
    }
}

// Networking needs to be up (e.g. wifi_start())
void acm_bridge_start(struct acm_bridge *s) {

    reset_peripheral();

    // FIXME: Some startup logic is necessary
    s->tcp_conn.sock = -1;


    device_disconnected_sem = xSemaphoreCreateBinary();
    assert(device_disconnected_sem);

    // Install USB Host driver. Should only be called once in entire application
    ESP_LOGI(TAG, "Installing USB Host");
    const usb_host_config_t host_config = {
        .skip_phy_setup = false,
        .intr_flags = ESP_INTR_FLAG_LEVEL1,
    };
    ESP_ERROR_CHECK(usb_host_install(&host_config));

    // Create a task that will handle USB library events
    BaseType_t task_created = xTaskCreate(
        acm_bridge_usb_lib_task, "usb_lib", 4096, s, USB_HOST_PRIORITY, NULL);
    assert(task_created == pdTRUE);

    ESP_LOGI(TAG, "Installing CDC-ACM driver");
    ESP_ERROR_CHECK(cdc_acm_host_install(NULL));


    esp_log_level_set("cdc_acm", ESP_LOG_DEBUG);

    // Create a task that will handle USB library events
    BaseType_t task_created1 = xTaskCreate(
        acm_bridge_acm_task, "usb_acm", 4096, s, USB_HOST_PRIORITY, NULL);
    assert(task_created1 == pdTRUE);

    // Start TCP server bridge to USB TTY ACM
    esp_tcp_listen(&s->tcp_conn);

}

struct acm_bridge node_bridge = {
    .tcp_conn = {
        .handle = node_loop,
        .port = 51400,
    }
};

#endif
