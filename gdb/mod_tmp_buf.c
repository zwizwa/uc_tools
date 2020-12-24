#ifndef MOD_TMP_BUF
#define MOD_TMP_BUF

/* Temporary buffer for console commands. */

#include "command.h"
#include "instance.h"

#ifndef TMP_BUF_SIZE
#define TMP_BUF_SIZE 64
#endif
struct pbuf tmp_buf_pbuf; uint8_t tmp_buf_pbuf_buf[TMP_BUF_SIZE];
DEF_COMMAND(clear) {
    pbuf_clear(&tmp_buf_pbuf);
}
void tmp_buf_write_uint(uint32_t nb) {
    uint8_t buf[nb];
    write_be(buf, command_stack_pop(), nb);
    pbuf_write(&tmp_buf_pbuf, buf, nb);
}
DEF_COMMAND(u8)   { tmp_buf_write_uint(1); }
DEF_COMMAND(u16)  { tmp_buf_write_uint(2); }
DEF_COMMAND(u32)  { tmp_buf_write_uint(4); }
DEF_COMMAND(tmp_buf) {
    struct pbuf *p = &tmp_buf_pbuf;
    infof("%d:", p->count);
    info_hex_u8(p->buf, p->count);
    infof("\n");
}

//DEF_COMMAND_NAMED("buf", tmp_buf_span) {
//    command_arg_push(tmp_buf.count);
//    command_arg_push((uintptr_t)tmp_buf.buf);
//}
instance_status_t tmp_buf_init(instance_init_t *ctx) {
    PBUF_INIT(tmp_buf_pbuf);
    return 0;
}
DEF_INSTANCE(tmp_buf);

#endif
