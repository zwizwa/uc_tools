#ifndef TESTFUNC_H
#define TESTFUNC_H

#include "macros.h"

#include <stdint.h>

static inline void log_hex_n(void *vbuf, uint32_t len) {
    uint8_t *buf = vbuf;
    for(uint32_t i=0; i<len; i++) { LOG(" %02x", buf[i]); }
    LOG("\n");
}

#endif
