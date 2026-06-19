#ifndef LOG_HEX_H
#define LOG_HEX_H

#include "macros.h"
#include <stdint.h>


static inline void log_hex(const uint8_t *buf, uint32_t len) {
    for (int i=0; i<len; i++) {
        if (i % 16 == 0) {
            LOG("%04x ", i);
        }
        LOG(" %02x", buf[i]);
        if (i % 16 == 7) {
           LOG(" ");
        }
        if (i % 16 == 15) {
            LOG("\n");
        }
    }
    if ((len % 16) != 0) {
        LOG("\n");
    }
}

#endif
