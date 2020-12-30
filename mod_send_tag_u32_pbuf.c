#ifndef MOD_SEND_TAG_U32
#define MOD_SEND_TAG_U32

/* Raw buffer writer. */
#ifndef SEND_TAG_U32_BUF_WRITE
#define SEND_TAG_U32_BUF_WRITE(...) port->write(__VA_ARGS__)
#endif

#include "tag_u32.h"
#include "pbuf.h"
#include "byteswap.h"

void send_tag_u32(
    void *context, /* Why is this here? */
    const uint32_t *arg,  uint32_t nb_args,
    const uint8_t *bytes, uint32_t nb_bytes) {

    uint32_t buf_size = 4 + 4 * nb_args + nb_bytes;
    uint8_t buf[buf_size];
    uint32_t n = 0;
    write_be(buf+n, TAG_U32, 2); n+=2;
    write_be(buf+n, nb_args, 2); n+=2;
    for (uint32_t i=0; i<nb_args; i++) {
        write_be(buf+n, arg[i], 4); n+=4;
    }
    memcpy(buf+n, bytes, nb_bytes); n+=nb_bytes;
    ASSERT(n == buf_size);
    SEND_TAG_U32_BUF_WRITE(port, buf, buf_size);
}

#endif
