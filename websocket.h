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

struct ws_message {
    uintptr_t len;
    uint8_t *buf;
    int mask:1;
    int fin:1;
    unsigned int opcode:4;
};

/* Decouple it from implementation of read and write. */
typedef uintptr_t ws_status_t;
struct ws_context;
struct ws_context {
    intptr_t (*read)(struct ws_context *, uint8_t *buf, uintptr_t len);
    intptr_t (*write)(struct ws_context *, const uint8_t *buf, uintptr_t len);
    ws_status_t (*push)(struct ws_context *, struct ws_message *);
};

/* Undo the xormask.  The xormask is (I believe), a hack that is part
   of the protocol to work around broken caching proxies. */
static inline void ws_unmask(uint8_t *key, uint8_t *buf, uintptr_t len) {
    for(uintptr_t i=0; i<len; i++) {
        buf[i] ^= key[i & 3];
    }
}
static inline ws_status_t ws_read_msg_body(struct ws_context *c, struct ws_message *m) {
    uint8_t key[4];
    uint8_t buf[m->len]; // FIXME: set a max size somewhere, or implement streaming
    m->buf = buf;
    intptr_t rv;
    if (sizeof(key) != (rv = c->read(c, key, sizeof(key)))) goto error_rv;
    ws_unmask(key, m->buf, m->len);
    return c->push(c, m);

  error_rv:
    LOG("error: %d\n", rv);
    return rv;
}
static inline ws_status_t ws_read_msg(struct ws_context *c) {
    /* We can read 2 bytes, then we have to dispatch on size. */
    uint8_t buf[2];
    intptr_t rv;
    if (2 != (rv = c->read(c, buf, 2))) goto error_rv;
    struct ws_message m = {};
    m.len        = buf[1] & 0x7f;
    m.opcode     = buf[0] & 0xf;
    m.mask       = 1 & (buf[1] >> 7);
    m.fin        = 1 & (buf[0] >> 7);

    if (m.len == 127) {
        /* Large message. */
        uint8_t len[8];
        if (sizeof(len) != (rv = c->read(c, len, sizeof(len)))) goto error_rv;
        m.len = read_be(len, sizeof(len));
        return ws_read_msg_body(c, &m);
    }
    else if (m.len == 126) {
        /* Medium size message. */
        uint8_t len[2];
        if (sizeof(len) != (rv = c->read(c, len, sizeof(len)))) goto error_rv;
        m.len = read_be(len, sizeof(len));
        return ws_read_msg_body(c, &m);
    }
    else {
        /* Small message, len <= 125 */
        return ws_read_msg_body(c, &m);
    }

  error_rv:
    LOG("error: %d\n", rv);
    return rv;

}


#endif
