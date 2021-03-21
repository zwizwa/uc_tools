#ifndef LOG_TOOLS_H
#define LOG_TOOLS_H

#include "macros.h"


static inline void log_u32(const char *tag, uint32_t nb, const uint32_t *arr) {
    LOG("%s",tag);
    for(int32_t i=0; i<nb; i++) { LOG(" %d", arr[i]); }
    LOG("\n");
}
static inline void log_hex(const char *tag, uint32_t nb, const uint8_t *buf) {
    LOG("%s",tag);
    for(int32_t i=0; i<nb; i++) { LOG(" %02x", buf[i]); }
    LOG("\n");
}


#endif
