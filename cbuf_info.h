#ifndef CBUF_INFO_H
#define CBUF_INFO_H

#include "cbuf.h"
#include "infof.h"

void cbuf_info_hex(struct cbuf *b) {
    uint32_t read  = b->read;
    uint32_t write = b->write;
    uint32_t elements = cbuf_elements3(b, read, write);
    for (uint32_t i=0; i<elements; i++) {
        infof(" %02x", b->buf[cbuf_index_(b, read, i)]);
    }
}



#endif
