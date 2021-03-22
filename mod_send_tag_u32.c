#ifndef MOD_SEND_TAG_U32
#define MOD_SEND_TAG_U32

/* Raw buffer writer. */
#ifndef SEND_TAG_U32_BUF_WRITE
#error need SEND_TAG_U32_BUF_WRITE
#endif

#include "tag_u32.h"
#include "pbuf.h"
#include "uct_byteswap.h"

#ifndef KEEP
#define KEEP
#endif

KEEP void send_reply_tag_u32(const struct tag_u32 *f, const struct tag_u32 *s) {
    uint32_t tot_nb_args = (f ? f->nb_from : 0) + s->nb_args;
    uint32_t buf_size = 4 + 4 * tot_nb_args + s->nb_bytes;
    uint8_t buf[buf_size];
    uint32_t n = 0;
    write_be(buf+n, TAG_U32, 2);  n+=2;
    buf[n] = 0;                   n+=1;
    buf[n] = tot_nb_args;         n+=1;
    if (f) {
        for (uint32_t i=0; i<f->nb_from; i++) {
            //LOG("from %08x %d\n", f->from[i], i);
            write_be(buf+n, f->from[i], 4); n+=4;
        }
    }
    for (uint32_t i=0; i<s->nb_args; i++) {
        write_be(buf+n, s->args[i], 4); n+=4;
    }
    memcpy(buf+n, s->bytes, s->nb_bytes); n+=s->nb_bytes;
    ASSERT(n == buf_size);

    //for(uint32_t i=0; i<buf_size; i++) { LOG(" %02x", buf[i]); } LOG("\n");

    SEND_TAG_U32_BUF_WRITE(buf, buf_size);
}

KEEP void send_tag_u32(const struct tag_u32 *s) {
    send_reply_tag_u32(NULL, s);
}

#endif
