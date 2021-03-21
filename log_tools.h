#ifndef LOG_TOOLS_H
#define LOG_TOOLS_H

#include "macros.h"


static inline void log_u32(const char *tag, const uint32_t *arr, uint32_t nb) {
    LOG("%s",tag);
    for(uint32_t i=0; i<nb; i++) { LOG(" %d", (int)arr[i]); }
    LOG("\n");
}
static inline void log_hex(const char *tag, const uint8_t *buf, uint32_t nb) {
    LOG("%s",tag);
    for(uint32_t i=0; i<nb; i++) { LOG(" %02x", buf[i]); }
    LOG("\n");
}


#endif
