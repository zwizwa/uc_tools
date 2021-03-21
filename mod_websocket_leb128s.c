#ifndef MOD_WEBSOCKET_LEB128S
#define MOD_WEBSOCKET_LEB128S

/* Implements the leb128s protocol on a websocket.  This briges
   external i/o handled by mod_webserver.c with an internal tag_u32
   directory tree handler. */

#ifndef WEBSOCKET_MSG_BUF
#define WEBSOCKET_MSG_BUF (4*1024) // FIXME: This will need to be managed differently
#endif


#include "leb128s.h"
#include "log_tools.h"
#include "websocket.h"
#include "tag_u32.h"
int handle_tag_u32(struct tag_u32 *req);

/* Return path. */
struct msg_ctx {
    struct tag_u32 msg;
};

void reply_tag_u32(const struct tag_u32 *req, const struct tag_u32 *rpl) {
    struct blocking_io *io = req->reply_ctx;

    uint8_t buf[WEBSOCKET_MSG_BUF];
    struct leb128s s = {
        .buf = buf,
        .len = sizeof(buf)
    };
    leb128s_write_i32(&s, T_TAG);              if(s.error) goto error;
    leb128s_write_tag_u32_reply(&s, req, rpl); if(s.error) goto error;
    //log_hex("enc: ", s.offset, buf);

    struct ws_message m = {
        .opcode = 2,  // binary
        .fin = 1,
        .mask = 0,
        .buf = buf,
        .len = s.offset,
    };
    ws_write_msg(io, &m);
    return;
  error:
    LOG("leb128 write error %x\n", s.error);
    return;
}

/* Incoming request from websocket. */
leb128s_status_t push_tag_u32(struct leb128s *s, struct tag_u32 *msg) {
    msg->reply = reply_tag_u32;
    msg->reply_ctx = s->env->ctx;
    if (0) {
        log_u32("from: ", msg->from,  msg->nb_from);
        log_u32("to:   ", msg->args,  msg->nb_args);
        log_hex("bin:  ", msg->bytes, msg->nb_bytes);
    }
    handle_tag_u32(msg);
    return 0;
}

ws_err_t websocket_push(struct blocking_io *io, struct ws_message *m) {

    // m->buf[m->len] = 0; // FIXME: don't do this
    // LOG("push: %s\n", m->buf);
    if (0) {
        log_hex("ws_push:", m->buf, m->len);
    }

    struct leb128s_env env = {
        .tag_u32 = push_tag_u32,
        .ctx = io,
    };

    struct leb128s s = { .buf = m->buf, .len = m->len, .env = &env };

    /* This calls the push_tag_u32 callback when a T_TAG message is
       received. */
    leb128_id_t id = leb128s_element(&s);

    (void)id;
    if(s.error) {
        LOG("error %d\n", s.error);
    }
    return 0;
}



#endif
