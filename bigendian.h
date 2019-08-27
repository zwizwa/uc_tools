#ifndef BIGENDIAN_H
#define BIGENDIAN_H

#include <stdint.h>

static inline uint64_t bigendian_read(const uint8_t *buf, uint32_t len) {
    uint64_t acc = 0;
    for (uint32_t i=0; i<len; i++) {
        acc = (acc << 8) + buf[i];
    }
    return acc;
}
static inline void bigendian_write(uint8_t *buf, uint32_t len, uint64_t val) {
    for (uint32_t i=0; i<len; i++) {
        buf[len-1-i] = val & 0xFF;
        val = val >> 8;
    }
}

#endif
