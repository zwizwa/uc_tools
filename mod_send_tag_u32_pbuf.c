#ifndef MOD_SEND_TAG_U32
#define MOD_SEND_TAG_U32

/* Raw buffer writer. */
#ifndef SEND_TAG_U32_BUF_WRITE
#define SEND_TAG_U32_BUF_WRITE(...) port->write(__VA_ARGS__)
#endif

#include "tag_u32.h"
#include "pbuf.h"
#include "byteswap.h"

void send_tag_u32(const struct tag_u32 *s) {

    uint32_t buf_size = 4 + 4 * s->nb_args + s->nb_bytes;
    uint8_t buf[buf_size];
    uint32_t n = 0;
    write_be(buf+n, TAG_U32, 2); n+=2;
    write_be(buf+n, s->nb_args, 2); n+=2;
    for (uint32_t i=0; i<s->nb_args; i++) {
        write_be(buf+n, s->args[i], 4); n+=4;
    }
    memcpy(buf+n, s->bytes, s->nb_bytes); n+=s->nb_bytes;
    ASSERT(n == buf_size);
    SEND_TAG_U32_BUF_WRITE(port, buf, buf_size);
}

#endif
