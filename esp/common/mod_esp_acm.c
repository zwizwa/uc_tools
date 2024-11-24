#ifndef MOD_CSP_ACM
#define MOD_CSP_ACM

#include "usb/usb_host.h"
#include "usb/cdc_acm_host.h"

// https://github.com/espressif/esp-idf/tree/f420609c332fbd2d2f7f188c6579d046c9560e42/examples/peripherals/usb/host/cdc/cdc_acm_host
#define USB_HOST_PRIORITY   (20)
#define USB_DEVICE_VID      (0x0483)
#define USB_DEVICE_PID      (0x5740)
#define TX_STRING           ("CDC test string!")
#define TX_TIMEOUT_MS       (3000)

static SemaphoreHandle_t device_disconnected_sem;

static bool acm_handle_rx(const uint8_t *data, size_t data_len, void *arg) {
    ESP_LOGI(TAG, "Data received");
    ESP_LOG_BUFFER_HEXDUMP(TAG, data, data_len, ESP_LOG_INFO);
    return true;
}
static void acm_handle_event(const cdc_acm_host_dev_event_data_t *event, void *user_ctx) {
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
static void usb_lib_task(void *arg) {
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
static void usb_acm_connect_task(void *arg) {

    const cdc_acm_host_device_config_t dev_config = {
        .connection_timeout_ms = TX_TIMEOUT_MS,
        .out_buffer_size = 512,
        .in_buffer_size = 512,
        .user_arg = NULL,
        .event_cb = acm_handle_event,
        .data_cb = acm_handle_rx
    };

    while (1) {
        cdc_acm_dev_hdl_t cdc_dev = NULL;

        // Open USB device from tusb_serial_device example example. Either single or dual port configuration.
        ESP_LOGI(TAG, "Opening CDC ACM device 0x%04X:0x%04X...", USB_DEVICE_VID, USB_DEVICE_PID);
        uint8_t interface_idx = 0;
        // uint8_t interface_idx = 1;
        esp_err_t err = cdc_acm_host_open(USB_DEVICE_VID, USB_DEVICE_PID, interface_idx, &dev_config, &cdc_dev);
        if (ESP_OK != err) {
            ESP_LOGI(TAG, "Failed to open device");
            continue;
        }
        cdc_acm_host_desc_print(cdc_dev);
        vTaskDelay(pdMS_TO_TICKS(100));

        // Test sending and receiving: responses are handled in handle_rx callback
        ESP_ERROR_CHECK(cdc_acm_host_data_tx_blocking(cdc_dev, (const uint8_t *)TX_STRING, strlen(TX_STRING), TX_TIMEOUT_MS));
        vTaskDelay(pdMS_TO_TICKS(100));


        // We are done. Wait for device disconnection and start over
        ESP_LOGI(TAG, "Example finished successfully! You can reconnect the device to run again.");
        xSemaphoreTake(device_disconnected_sem, portMAX_DELAY);
    }
}




void acm_start(void) {

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
    BaseType_t task_created = xTaskCreate(usb_lib_task, "usb_acm_connect_task", 4096, xTaskGetCurrentTaskHandle(), USB_HOST_PRIORITY, NULL);
    assert(task_created == pdTRUE);

    ESP_LOGI(TAG, "Installing CDC-ACM driver");
    ESP_ERROR_CHECK(cdc_acm_host_install(NULL));


    esp_log_level_set("cdc_acm", ESP_LOG_DEBUG);

    // Create a task that will handle USB library events
    BaseType_t task_created1 = xTaskCreate(usb_acm_connect_task, "usb_lib", 4096, xTaskGetCurrentTaskHandle(), USB_HOST_PRIORITY, NULL);
    assert(task_created1 == pdTRUE);


}

#endif
