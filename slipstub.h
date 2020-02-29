#ifndef SLIPSTUB_H
#define SLIPSTUB_H

#include "cbuf.h"
#include "pbuf.h"

struct slipstub;
typedef void (*slipstub_dispatch_t)(struct slipstub *s, uint16_t tag, const struct pbuf *p);

struct slipstub {
    struct cbuf *slip_in;
    struct pbuf *packet_in;
    struct cbuf *slip_out;
    slipstub_dispatch_t dispatch;
};

void slipstub_switch_protocol(const uint8_t *buf, uint32_t size);

#endif
