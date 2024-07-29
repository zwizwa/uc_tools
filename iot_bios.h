#ifndef IOT_BIOS
#define IOT_BIOS

/* Original idea: a fleet of esp32 chips has firmware code split into
   two parts: a BIOS in Flash developed using ESP IDF, and application
   code in SRAM that can change on the fly.  The BIOS provides a set
   of calls, and exposes a socket that can be used to upload code.
   This allows the application to be kept platform-independent, and
   also provides a quick mechanism to update application code.  The
   idea is that IOT code always talks to the server, so the server
   might just as well push code on each startup. */

// esp-idf specifics, hide these elsewhere
typedef esp_partition_t  iot_partition_t;
typedef esp_ota_handle_t iot_ota_handle_t;
typedef esp_err_t        iot_err_t;

/* Modeled after the first instance wich is implemented using esp_ota_*  */
struct iot_ota {
    int (*get_sha256)(char *dst, size_t size);
    iot_err_t (*begin)(const iot_partition_t *partition, size_t image_size, iot_ota_handle_t *out_handle);
    iot_err_t (*write)(iot_ota_handle_t handle, const void *data, size_t size);
    iot_err_t (*end)(iot_ota_handle_t handle);
};

struct iot_bios {
    void *(*malloc)(size_t size);
    int (*printf)(const char*, ...);
    void (*reboot)(void);
    const struct iot_ota *iot_ota;
};

#endif
