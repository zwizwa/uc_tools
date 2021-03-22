#ifndef MOD_TAG_U32_STREAM
#define MOD_TAG_U32_STREAM

/* Main loop for tag_u32 on stdio, with support for data streaming. */

#include "assert_write.h"
#include "assert_read.h"
#include "uct_byteswap.h"
#include "tag_u32.h"
void write_packet(const uint8_t *buf, uint32_t buf_size) {
    uint8_t hdr[4] = {U32_BE(buf_size)};
    assert_write(1, hdr, 4);
    assert_write(1, buf, buf_size);
}
#define SEND_TAG_U32_BUF_WRITE(...) write_packet( __VA_ARGS__)
static inline uint32_t read_uint(uint32_t nb) {
    uint8_t buf[nb];
    assert_read(0, buf, nb);
    return read_be(buf, nb);
}
#define READ_VAR(type,var) \
        type var = read_uint(sizeof(var))

int handle_tag_u32(struct tag_u32 *req);

/* The main thread handles the communication. */
int tag_u32_loop(void) {
    /* Streaming parser. */
    for(;;) {
        READ_VAR(uint32_t,len);
        READ_VAR(uint16_t,tag); ASSERT(TAG_U32 == tag);
        READ_VAR(uint8_t,nb_from);
        READ_VAR(uint8_t,nb_args);
        uint32_t nb_tags = nb_from + nb_args;
        uint32_t nb_bytes = len - 4 - 4 * nb_tags;
        //LOG("%d %d %d %d %d:", len, tag, nb_from, nb_args, nb_bytes);
        uint32_t tags[nb_tags];
        for (uint32_t i=0; i<nb_tags; i++) {
            tags[i] = read_uint(4);
            //LOG(" %d", tags[i]);
        }
        //LOG("\n");

        struct tag_u32 s = {
            .reply = send_reply_tag_u32,
            .from = tags,         .nb_from = nb_from,
            .args = tags+nb_from, .nb_args = nb_args,
            .nb_bytes = nb_bytes
        };
        // FIXME: how to decide?
        if (1) {
            uint8_t buf[nb_bytes + 1];
            s.bytes = buf;
            assert_read(0,buf,nb_bytes);
            buf[nb_bytes] = 0;
            //LOG("buf = %s\n", buf);
            handle_tag_u32(&s);
        }
    }
}



#endif
