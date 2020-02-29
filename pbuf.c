#include "pbuf.h"
/* Collector for incoming {packet,N} Erlang format.  Keep the length
   prefix in the buffer to avoid extra state tracking. */
void pbuf_packetn_write(struct pbuf *p, int len_len,
                        const uint8_t *buf, uint32_t len,
                        pbuf_sink_t sink, void *ctx) {
    for (uint32_t i=0; i<len; i++) {
        pbuf_put(p, buf[i]);
        if (p->count >= len_len) {
            uint32_t count = read_be(&p->buf[0], len_len);
            if (p->count == count + 4) {
                if (sink) sink(ctx, p);
                p->count = 0;
            }
        }
    }
}

/* incoming -> slip cbuf -> flat pbuf -> sink */
void pbuf_slip_write(
    const uint8_t *buf, uint32_t len, // input buffer
    struct cbuf *c,                   // stage 1 circular buffer for slip stream
    struct pbuf *p,                   // stage 2 flat decoded packet buffer
    pbuf_sink_t sink, void *ctx) {    // handler of stage2 buffer

    /* Read the cbuf immediately after writing so the cbuf can be kept
       small.  It contains max 2 characters, so a buffer of 4 is
       sufficient -- one slot is never used to distinguish full from
       empty, and size needs to be power of two. */
    for(uint32_t i=0; i<len; i++) {
        cbuf_put(c, buf[i]);
        uint16_t fc;
        while (CBUF_EAGAIN != (fc = cbuf_get_slip_decode(c))) {
            if (CBUF_OOB(SLIP_END) == fc) {
                if (p->count) sink(ctx, p);
                p->count = 0;
            }
            else if (fc >= 0x100) {
                /* Out-of-band characters other than frame borders are not
                 * expected.  Not much that can be done here. */
                infof("oob %d\n", fc);
            }
            else {
                /* Normal characters get collected until buffer is full. */
                pbuf_put(p, fc);
            }
        }
    }
}

void cbuf_write_slip_reply(
    struct cbuf *c,
    const struct pbuf *p,
    uint32_t offset,
    uint8_t *buf,
    uint32_t len) {

    uint8_t tagbuf[] = {TAG_REPLY >> 8, TAG_REPLY & 0xff};
    struct slice slices[] = {
        {tagbuf, sizeof(tagbuf)},
        /* Continuation, passed in by caller and copied out. */
        {p->buf + offset, p->count - offset},
        /* Reply data */
        {buf, len}
    };
    cbuf_write_slip_slices(c, slices, 3);
}
