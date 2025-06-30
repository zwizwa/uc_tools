#ifndef WEBSOCKET_H
#define WEBSOCKET_H

/* Minimalistic implementation of the WebSocket protocol.
   Derived from erl_tools/src/serv_ws.erl

   1. struct blocking_io mode:

   Dependencies / assumptions:

   - assumes blocking read/write are available

   - assumes there is no need for "chunking" the read and write.
     E.g. if single byte reads are too inefficient, upstream will need
     to implement the buffering.

   - message buffers are allocated on the stack and passed to a callback.

   2. single-threaded try/abort mode:

   Further decouple everything to allow this to be used in a setjmp
   based try/abort mode, e.g. for use in a single-threaded Lua app.

*/


#include <stdint.h>
#include "macros.h"

#include "sha1.h"
#include "base64.h"
#include "uct_byteswap.h"

#include <stdint.h>

/* In pure mode the ties to blocking_io and the rest of the C http
   server are not made. */
#ifndef WS_PURE
#include "httpserver.h"
#else

#ifndef WS_ERR_T
#define WS_ERR_T uint32_t
#endif

#define WS_READ(io, buf, len) { if(0) goto error_exit; ws_buf_read(io, buf, len); }
#define WS_WRITE(io, buf, len) { ws_buf_write(io, buf, len); if(0) goto error_exit; }
#define WS_OK 0
#define WS_IO_T struct ws_buf
#define WS_LOG_ERROR LOG
#include <setjmp.h>
#include "cbuf.h"
struct ws_buf {
    jmp_buf jmp_buf;
    struct cbuf in;   uint8_t in_buf[1024];
    /* This can just be a temp buffer on the stack that lives during
       the extent of a ws_write_msg_nolock() call just to create the
       encoded message in the out buffer.  */
    struct cbuf *out;
};
static inline void ws_buf_init(struct ws_buf *ws_io) {
    CBUF_INIT(ws_io->in);
}
WS_ERR_T ws_buf_read(struct ws_buf *ws_io, uint8_t *buf, size_t len) {
    uint32_t nb_bytes = cbuf_elements(&ws_io->in);
    if (nb_bytes < len) {
        /* Short read. */
        longjmp(ws_io->jmp_buf, 1);
    }
    cbuf_read(&ws_io->in, buf, len);
    return WS_OK;
}
WS_ERR_T ws_buf_write(struct ws_buf *ws_io, const uint8_t *buf, size_t len) {
    ASSERT(ws_io->out);
    uint32_t room = cbuf_room(ws_io->out);
    if (room < len) {
        /* Buffer overflow. */
        longjmp(ws_io->jmp_buf, 1);
    }
    cbuf_write(ws_io->out, buf, len);
    return WS_OK;
}

#endif


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


static inline void ws_write_sha1(const char *key, uint8_t *sha1_buf) {
    SHA1_CTX ctx;
    sha1_init(&ctx);
    sha1_update(&ctx, (uint8_t*)key, strlen(key));
    const char magic[] = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";
    sha1_update(&ctx, (uint8_t*)magic, strlen(magic));
    sha1_final(&ctx, sha1_buf);
}

/* Undo the xormask.  The xormask is (I believe), a hack that is part
   of the protocol to work around broken caching proxies. */
static inline void ws_unmask(struct ws_message *m) {
    for(uintptr_t i=0; i<m->len; i++) {
        m->buf[i] ^= m->xorkey[i & 3];
    }
}



/* By default the ws reading code assumes struct blocking_io
   interface.  For a single threaded try/abort implementation see
   webserver_lua51.c */
#ifndef WS_READ
#define WS_READ  BLOCKING_IO_READ
#endif
#ifndef WS_WRITE
#define WS_WRITE BLOCKING_IO_WRITE
#endif

#ifndef WS_ERR_T
#define WS_ERR_T os_error_t
#endif
typedef WS_ERR_T ws_err_t;

#ifndef WS_IO_T
#define WS_IO_T struct blocking_io
#endif

#ifndef WS_OK
#define WS_OK OS_OK
#endif

#ifndef WS_LOG_ERROR
#define WS_LOG_ERROR OS_LOG_ERROR
#endif

typedef WS_IO_T ws_io_t;
typedef ws_err_t (*ws_push_fn)(ws_io_t *, struct ws_message *);


static inline ws_err_t ws_write_msg_nolock(ws_io_t *io,
                                           const struct ws_message *m) {
    ws_err_t error = WS_OK;

    if (m->len < 126) {
        uint8_t hdr[2] = {
            (m->fin << 7) | (m->opcode & 0xF),
            (m->mask << 7) | m->len
        };
        WS_WRITE(io, hdr, 2);
    }
    else if (m->len < 0xFFFF) {
        uint8_t hdr[4] = {
            (m->fin << 7) | (m->opcode & 0xF),
            (m->mask << 7) | 126 /* medium size code */
        };
        write_be(hdr + 2, m->len, 2);
        WS_WRITE(io, hdr, 4);
    }
    else {
        uint8_t hdr[10] = {
            (m->fin << 7) | (m->opcode & 0xF),
            (m->mask << 7) | 127 /* large size code */
        };
        write_be(hdr + 2, m->len, 8);
        WS_WRITE(io, hdr, 10);
    }
    WS_WRITE(io, m->buf, m->len);
    return WS_OK;

  error_exit:
    WS_LOG_ERROR("ws_write_msg %d", error);
    return error;
}




static inline ws_err_t ws_read_msg_body(ws_io_t *io,
                                        ws_push_fn push,
                                        struct ws_message *m,
                                        uintptr_t len_len) {
    ws_err_t error = WS_OK;
    if (len_len) {
        /* Large and Medium have an additional length field. */
        uint8_t len_buf[len_len];
        WS_READ(io, len_buf, len_len);
        m->len = read_be(len_buf, len_len);
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

        WS_READ(io, m->xorkey, 4);
        WS_READ(io, m->buf, m->len);
        ws_unmask(m);
        if (m->opcode == 2) {
            /* Binary */
            return push(io, m);
        }
        if (m->opcode == 8) {
            LOG("closing websocket\n");
            return WS_OK;
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

  error_exit:
    WS_LOG_ERROR("ws_read_msg_body %d", error);
    return error;
}
static inline ws_err_t ws_read_msg(ws_io_t *io, ws_push_fn push) {
    ws_err_t error = WS_OK;

    /* We can read 2 bytes, then we have to dispatch on size. */
    uint8_t buf[2];
    WS_READ(io, buf, 2);
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

  error_exit:
    WS_LOG_ERROR("ws_read_msg %d", error);
    return error;
}





#ifndef WS_PURE
static inline ws_err_t ws_write_msg(ws_io_t *io,
                                    const struct ws_message *m) {
    os_mutex_lock(&io->write_lock);
    ws_err_t rv = ws_write_msg_nolock(io, m);
    os_mutex_unlock(&io->write_lock);
    return rv;
}

#else

/* Interface for try/abort style.
   Saves bytes into buffer and calls the push function for each message. */
static inline ws_err_t ws_update(ws_io_t *io,
                                 const uint8_t *buf, size_t len,
                                 ws_push_fn push) {
    cbuf_write(&io->in, buf, len);
    for(;;) {
        typeof(io->in.read) read = io->in.read;
        int rv;
        if ((rv = setjmp(io->jmp_buf))) {
            /* Short read: rewind */
            // LOG("ws:update rewind %d from %d\n", read, io->in.read);
            io->in.read = read;
            return WS_OK;
        }
        ws_err_t err = ws_read_msg(io, push);
        if (err != WS_OK) return err;
    }
}




#endif



#endif
