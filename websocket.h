#ifndef WEBSOCKET_H
#define WEBSOCKET_H

/* Minimalistic implementation of the WebSocket protocol.
   Derived from erl_tools/src/serv_ws.erl

   Dependencies / assumptions:

   - assumes blocking read/write are available

   - assumes there is no need for "chunking" the read and write.
     E.g. if single byte reads are too inefficient, upstream will need
     to implement the buffering.

   - message buffers are allocated on the stack and passed to a callback.

*/

#include <stdint.h>
#include "byteswap.h"
#include "macros.h"
#include "httpserver.h"

struct ws_message {
    uintptr_t len;
    uint8_t xorkey[4];
    uint8_t *buf;
    int mask:1;
    int fin:1;
    unsigned int opcode:4;
};

/* Set this to the maximum message size used by the application.
   Currently streaming is not supported so don't make this too big. */
#define WS_LEN_MAX 4096

/* Decouple it from implementation of read and write. */
typedef uintptr_t ws_err_t; struct ws_req;
struct ws_req {
    struct http_req c;
    ws_err_t (*push)(struct ws_req *, struct ws_message *);
};

/* Undo the xormask.  The xormask is (I believe), a hack that is part
   of the protocol to work around broken caching proxies. */
static inline void ws_unmask(struct ws_message *m) {
    for(uintptr_t i=0; i<m->len; i++) {
        m->buf[i] ^= m->xorkey[i & 3];
    }
}
static inline ws_err_t ws_read_msg_body(struct ws_req *c, struct ws_message *m,
                                           uintptr_t len_len) {
    uintptr_t rv = 0;
    if (len_len) {
        /* Large and Medium have an additional length field. */
        uint8_t len[len_len];
        if (len_len != (rv = c->c.read(&c->c, len, len_len))) goto error_rv;
        m->len = read_be(len, len_len);
    }
    else {
        /* For Small, there is no additional length field, and m->len
           is already correct. */
    }
    {
        /* Buffer goes on the stack.  FIXME: Is streaming necessary?
           It's a little awkward because we need to undo the xor
           mask, so we would need to provide a read function. */
        ASSERT(m->len <= WS_LEN_MAX);
        uint8_t buf[m->len]; m->buf = buf;

        if (4 != (rv = c->c.read(&c->c, m->xorkey, 4))) goto error_rv;
        if (m->len != (rv = c->c.read(&c->c, m->buf, m->len))) goto error_rv;
        ws_unmask(m);
        return c->push(c, m);
    }

  error_rv:
    LOG("error: %d\n", rv);
    return rv;
}
static inline ws_err_t ws_read_msg(struct ws_req *c) {
    /* We can read 2 bytes, then we have to dispatch on size. */
    uint8_t buf[2];
    intptr_t rv;
    if (2 != (rv = c->c.read(&c->c, buf, 2))) goto error_rv;
    struct ws_message m = {};
    m.len        = buf[1] & 0x7f;
    m.opcode     = buf[0] & 0xf;
    m.mask       = 1 & (buf[1] >> 7);
    m.fin        = 1 & (buf[0] >> 7);

    if (m.len == 127) {
        /* Large message. */
        return ws_read_msg_body(c, &m, 8);
    }
    else if (m.len == 126) {
        /* Medium size message. */
        return ws_read_msg_body(c, &m, 2);
    }
    else {
        /* Small message, len <= 125 */
        return ws_read_msg_body(c, &m, 0 /* already have m.len */);
    }

  error_rv:
    LOG("error: %d\n", rv);
    return rv;

}


#endif
