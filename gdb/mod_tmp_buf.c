#ifndef MOD_TMP_BUF_C
#define MOD_TMP_BUF_C

/* Temporary buffer for console commands. */

#include "command.h"
#ifndef TMP_BUF_SIZE
#define TMP_BUF_SIZE 64
#endif
struct pbuf tmp_buf; uint8_t tmp_buf_buf[TMP_BUF_SIZE];
DEF_COMMAND(clear) {
    pbuf_clear(&tmp_buf);
}
void tmp_buf_write_uint(uint32_t nb) {
    uint8_t buf[nb];
    write_be(buf, command_stack_pop(), nb);
    pbuf_write(&tmp_buf, buf, nb);
}
DEF_COMMAND(u8)   { tmp_buf_write_uint(1); }
DEF_COMMAND(u16)  { tmp_buf_write_uint(2); }
DEF_COMMAND(u32)  { tmp_buf_write_uint(4); }

//DEF_COMMAND_NAMED("buf", tmp_buf_span) {
//    command_arg_push(tmp_buf.count);
//    command_arg_push((uintptr_t)tmp_buf.buf);
//}
void tmp_buf_init(void) {
    PBUF_INIT(tmp_buf);
}
#endif
