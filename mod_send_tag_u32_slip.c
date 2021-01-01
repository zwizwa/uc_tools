#ifndef MOD_SEND_TAG_U32
#define MOD_SEND_TAG_U32

#ifndef SEND_TAG_U32_CBUF
#define SEND_TAG_U32_CBUF slipstub.slip_out
#endif

#include "tag_u32.h"
#include "cbuf.h"

KEEP void send_tag_u32(const struct tag_u32 *s) {

    uint8_t hdr[] = {U16_BE(TAG_U32), U16_BE(s->nb_args)};
    struct cbuf *b = SEND_TAG_U32_CBUF;
    cbuf_put(b, SLIP_END);
    cbuf_append_slip(b, hdr, sizeof(hdr));
    for (uint32_t i=0; i<s->nb_args; i++) {
        uint8_t a[] = {U32_BE(s->args[i])};
        cbuf_append_slip(b, a, sizeof(a));
    }
    cbuf_append_slip(b, s->bytes, s->nb_bytes);
    cbuf_put(b, SLIP_END);
}


#endif
