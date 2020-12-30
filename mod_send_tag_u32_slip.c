#ifndef MOD_SEND_TAG_U32
#define MOD_SEND_TAG_U32

#ifndef SEND_TAG_U32_CBUF
#define SEND_TAG_U32_CBUF slipstub.slip_out
#endif

#include "tag_u32.h"
#include "cbuf.h"

KEEP void send_tag_u32(
    void *context, /* Why is this here? */
    const uint32_t *arg,  uint32_t nb_args,
    const uint8_t *bytes, uint32_t nb_bytes) {

    uint8_t hdr[] = {U16_BE(TAG_U32), U16_BE(nb_args)};
    struct cbuf *b = SEND_TAG_U32_CBUF;
    cbuf_put(b, SLIP_END);
    cbuf_append_slip(b, hdr, sizeof(hdr));
    for (uint32_t i=0; i<nb_args; i++) {
        uint8_t a[] = {U32_BE(arg[i])};
        cbuf_append_slip(b, a, sizeof(a));
    }
    cbuf_append_slip(b, bytes, nb_bytes);
    cbuf_put(b, SLIP_END);
}


#endif
