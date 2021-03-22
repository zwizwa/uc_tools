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
#include "uct_byteswap.h"
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
typedef uintptr_t ws_err_t; struct ws_ctx;
typedef ws_err_t (*ws_push_fn)(struct blocking_io *, struct ws_message *);

/* Undo the xormask.  The xormask is (I believe), a hack that is part
   of the protocol to work around broken caching proxies. */
static inline void ws_unmask(struct ws_message *m) {
    for(uintptr_t i=0; i<m->len; i++) {
        m->buf[i] ^= m->xorkey[i & 3];
    }
}
static inline ws_err_t ws_read_msg_body(struct blocking_io *io,
                                        ws_push_fn push,
                                        struct ws_message *m,
                                        uintptr_t len_len) {
    uintptr_t rv = 0;
    if (len_len) {
        /* Large and Medium have an additional length field. */
        uint8_t len[len_len];
        if (len_len != (rv = io->read(io, len, len_len))) goto error_rv;
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

        if (4 != (rv = io->read(io, m->xorkey, 4))) goto error_rv;
        if (m->len != (rv = io->read(io, m->buf, m->len))) goto error_rv;
        ws_unmask(m);
        if (m->opcode == 2) {
            /* Binary */
            return push(io, m);
        }
        if (m->opcode == 8) {
            LOG("closing websocket\n");
            return 0;
        }
        /* https://tools.ietf.org/id/draft-ietf-hybi-thewebsocketprotocol-09.html
           0x0 denotes a continuation frame
           0x1 denotes a text frame
           0x2 denotes a binary frame
           0x3-0x7 are reserved for further non-control frames
           0x8 denotes a connection close
           0x9 denotes a ping
           0xA denotes a pong
           0xB-F are reserved for further control frames */
        LOG("unsupported opcode %d\n", m->opcode);
    }

  error_rv:
    LOG("error: %d\n", rv);
    return rv;
}
static inline ws_err_t ws_read_msg(struct blocking_io *io, ws_push_fn push) {

    /* We can read 2 bytes, then we have to dispatch on size. */
    uint8_t buf[2];
    intptr_t rv;
    if (2 != (rv = io->read(io, buf, 2))) goto error_rv;
    struct ws_message m = {};
    m.len        = buf[1] & 0x7f;
    m.opcode     = buf[0] & 0xf;
    m.mask       = 1 & (buf[1] >> 7);
    m.fin        = 1 & (buf[0] >> 7);

    if (m.len == 127) {
        /* Large message. */
        return ws_read_msg_body(io, push, &m, 8);
    }
    else if (m.len == 126) {
        /* Medium size message. */
        return ws_read_msg_body(io, push, &m, 2);
    }
    else {
        /* Small message, len <= 125 */
        return ws_read_msg_body(io, push, &m, 0 /* already have m.len */);
    }

  error_rv:
    LOG("error: %d\n", rv);
    return rv;

}

static inline ws_err_t ws_write_msg(struct blocking_io *io, const struct ws_message *m) {
    intptr_t rv;
    if (m->len < 126) {
        uint8_t hdr[2] = {
            (m->fin << 7) | (m->opcode & 0xF),
            (m->mask << 7) | m->len
        };
        if (2 != (rv = io->write(io, hdr, 2))) goto error_rv;
    }
    else if (m->len < 0xFFFF) {
        uint8_t hdr[4] = {
            (m->fin << 7) | (m->opcode & 0xF),
            (m->mask << 7) | 126 /* medium size code */
        };
        write_be(hdr + 2, m->len, 2);
        if (4 != (rv = io->write(io, hdr, 4))) goto error_rv;
    }
    else {
        uint8_t hdr[10] = {
            (m->fin << 7) | (m->opcode & 0xF),
            (m->mask << 7) | 127 /* large size code */
        };
        write_be(hdr + 2, m->len, 8);
        if (10 != (rv = io->write(io, hdr, 10))) goto error_rv;
    }
    if (m->len != (typeof(m->len))
        (rv = io->write(io, m->buf, m->len))) goto error_rv;
    rv = 0;
  error_rv:
    if (rv) LOG("error: %d\n", rv);
    return rv;
}


#endif
