#ifndef ESP_OS
#define ESP_OS

#include <stddef.h>
#include <stdint.h>

/* Functionality exposed to 3if monitor. */

struct esp_os {
    void *(*malloc)(size_t size);
    int (*printf)(const char*, ...);
    void (*log_u32)(uint32_t);
};


#endif
