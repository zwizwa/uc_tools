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

#include "usb/usb_host.h"
#include "usb/cdc_acm_host.h"



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


// https://github.com/espressif/esp-idf/tree/f420609c332fbd2d2f7f188c6579d046c9560e42/examples/peripherals/usb/host/cdc/cdc_acm_host
#define EXAMPLE_USB_HOST_PRIORITY   (20)
#define EXAMPLE_USB_DEVICE_VID      (0x0483)
#define EXAMPLE_USB_DEVICE_PID      (0x5740)
#define EXAMPLE_TX_STRING           ("CDC test string!")
#define EXAMPLE_TX_TIMEOUT_MS       (3000)

static SemaphoreHandle_t device_disconnected_sem;

static bool handle_rx(const uint8_t *data, size_t data_len, void *arg) {
    ESP_LOGI(TAG, "Data received");
    ESP_LOG_BUFFER_HEXDUMP(TAG, data, data_len, ESP_LOG_INFO);
    return true;
}
static void handle_event(const cdc_acm_host_dev_event_data_t *event, void *user_ctx) {
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
void start_usb(void) {

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
    BaseType_t task_created = xTaskCreate(usb_lib_task, "usb_lib", 4096, xTaskGetCurrentTaskHandle(), EXAMPLE_USB_HOST_PRIORITY, NULL);
    assert(task_created == pdTRUE);

    ESP_LOGI(TAG, "Installing CDC-ACM driver");
    ESP_ERROR_CHECK(cdc_acm_host_install(NULL));


    esp_log_level_set("cdc_acm", ESP_LOG_DEBUG);

    const cdc_acm_host_device_config_t dev_config = {
        .connection_timeout_ms = EXAMPLE_TX_TIMEOUT_MS,
        .out_buffer_size = 512,
        .in_buffer_size = 512,
        .user_arg = NULL,
        .event_cb = handle_event,
        .data_cb = handle_rx
    };

    while (true) {
        cdc_acm_dev_hdl_t cdc_dev = NULL;

        // Open USB device from tusb_serial_device example example. Either single or dual port configuration.
        ESP_LOGI(TAG, "Opening CDC ACM device 0x%04X:0x%04X...", EXAMPLE_USB_DEVICE_VID, EXAMPLE_USB_DEVICE_PID);
        uint8_t interface_idx = 0;
        // uint8_t interface_idx = 1;
        esp_err_t err = cdc_acm_host_open(EXAMPLE_USB_DEVICE_VID, EXAMPLE_USB_DEVICE_PID, interface_idx, &dev_config, &cdc_dev);
        if (ESP_OK != err) {
            ESP_LOGI(TAG, "Failed to open device");
            continue;
        }
        cdc_acm_host_desc_print(cdc_dev);
        vTaskDelay(pdMS_TO_TICKS(100));

        // Test sending and receiving: responses are handled in handle_rx callback
        ESP_ERROR_CHECK(cdc_acm_host_data_tx_blocking(cdc_dev, (const uint8_t *)EXAMPLE_TX_STRING, strlen(EXAMPLE_TX_STRING), EXAMPLE_TX_TIMEOUT_MS));
        vTaskDelay(pdMS_TO_TICKS(100));

#if 0
        // FIXME: I think the STM CDC ACM doesn't support these, at least the ESP throws an error:
        //
        // I (1095) app: Setting up line coding
        // E (1095) USBH: Dev 1 EP 0 STALL
        // E (1095) cdc_acm: cdc_acm_host_send_custom_request(1074): Control transfer error
        // E (1095) cdc_acm: cdc_acm_host_line_coding_get(989):

        // Test Line Coding commands: Get current line coding, change it 9600 7N1 and read again
        ESP_LOGI(TAG, "Setting up line coding");

        cdc_acm_line_coding_t line_coding;
        ESP_ERROR_CHECK(cdc_acm_host_line_coding_get(cdc_dev, &line_coding));
        ESP_LOGI(TAG, "Line Get: Rate: %"PRIu32", Stop bits: %"PRIu8", Parity: %"PRIu8", Databits: %"PRIu8"",
                 line_coding.dwDTERate, line_coding.bCharFormat, line_coding.bParityType, line_coding.bDataBits);

        line_coding.dwDTERate = 9600;
        line_coding.bDataBits = 7;
        line_coding.bParityType = 1;
        line_coding.bCharFormat = 1;
        ESP_ERROR_CHECK(cdc_acm_host_line_coding_set(cdc_dev, &line_coding));
        ESP_LOGI(TAG, "Line Set: Rate: %"PRIu32", Stop bits: %"PRIu8", Parity: %"PRIu8", Databits: %"PRIu8"",
                 line_coding.dwDTERate, line_coding.bCharFormat, line_coding.bParityType, line_coding.bDataBits);

        ESP_ERROR_CHECK(cdc_acm_host_line_coding_get(cdc_dev, &line_coding));
        ESP_LOGI(TAG, "Line Get: Rate: %"PRIu32", Stop bits: %"PRIu8", Parity: %"PRIu8", Databits: %"PRIu8"",
                 line_coding.dwDTERate, line_coding.bCharFormat, line_coding.bParityType, line_coding.bDataBits);

        ESP_ERROR_CHECK(cdc_acm_host_set_control_line_state(cdc_dev, true, false));
#endif

        // We are done. Wait for device disconnection and start over
        ESP_LOGI(TAG, "Example finished successfully! You can reconnect the device to run again.");
        xSemaphoreTake(device_disconnected_sem, portMAX_DELAY);
    }
}


void app_main(void)
{

    start_usb();


    start_wifi();

    // Memory info for host side plugin linker.

    // Use a dedicated buffer
    meminfo.ram_addr = (uint32_t)dram_buf;
    meminfo.ram_len = sizeof(dram_buf);

    // Use a dedicated buffer
    meminfo.flash_addr = (uint32_t)iram_buf;
    meminfo.flash_len = iram_buf_size;

    start_monitor();
}


