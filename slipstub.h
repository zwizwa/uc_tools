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

#ifndef SLIPSTUB_CBUF_SIZE
#define SLIPSTUB_CBUF_SIZE 1024
#endif
#ifndef SLIPSTUB_PBUF_SIZE
#define SLIPSTUB_PBUF_SIZE 1024
#endif

/* Typical buffer setup. */
struct slipstub_buffers {
    struct cbuf cbuf_from_usb; uint8_t cbuf_from_usb_buf[4];
    struct cbuf cbuf_to_usb;   uint8_t cbuf_to_usb_buf[SLIPSTUB_CBUF_SIZE];
    struct pbuf pbuf_from_usb; uint8_t pbuf_from_usb_buf[SLIPSTUB_PBUF_SIZE];
};

void slipstub_init(slipstub_dispatch_t dispatch);


#endif
