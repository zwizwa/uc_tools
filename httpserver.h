#ifndef HTTPSERVER_H
#define HTTPSERVER_H

/* Minimalistic HTTP server implementation.
   This is intended to serve some simple pages and to bootstrap a websocket.

   Temporary buffers are allocated on the stack, and data is passed to
   a callback in parsed form.  ( Fold instead of data. )
*/

#include "macros.h"

#include "blocking_io.h"

typedef intptr_t http_err_t;

struct http_req;
typedef void     (*http_close)(struct http_req *);


struct http_req {
    http_err_t (*request)(struct http_req *, int method, const char *uri);
    http_err_t (*header)(struct http_req *, const char *header, const char *value);
    /* The i/o is abstract.  This is done essentially for websockets,
       which only need socket state.  This way the socket can be
       allocated separately in a memory pool, and transient http state
       can be kept on the stack. */
    struct blocking_io *io;
};


void log_c(const uint8_t *buf, uintptr_t len) {
    for(uintptr_t i=0; i<len; i++) LOG("%c", buf[i]);
}

#define HTTP_STATUS_EOF            -1
#define HTTP_STATUS_OVERRUN        -2
#define HTTP_STATUS_INVALID        -3

#define HTTP_METHOD_GET 1
#define HTTP_METHOD_PUT 2


/* Don't do anything fancy, just read the headers into a buffer. */
static inline http_err_t http_read_headers(struct http_req *c) {
    /* We parse one line at a time, which allows us to keep the buffer
       small, and locally managed.  Callbacks can use the memory in
       the extent of the call. */
    char buf[4096];
    uintptr_t len = 0;
    intptr_t rv;
    uintptr_t nb_lines = 0;
    for(;;) {
        if (len >= sizeof(buf)) return HTTP_STATUS_OVERRUN;
        rv = c->io->read(c->io, (uint8_t*)&buf[len++], 1);
        if (rv != 1) {
            LOG("http_read_headers rv = %d\n", rv);
            return HTTP_STATUS_EOF;
        }
        if ((len >= 2) && (buf[len-2] == 0xd) && (buf[len-1] == 0xa)) {
            /* Convert 0d,0a to cstring. */
            len -= 2; buf[len] = 0;
            if (len == 0) { break; }
            if (0 == nb_lines) {
                /* Only HTTP/1.1 GET is supported. */
                rv = HTTP_STATUS_INVALID;
                if (len < (3+1+1+1+8)) { goto error;};
                int method;
                if (!strncmp(buf,"GET ",3+1)) {
                    method = HTTP_METHOD_GET;
                }
                else if (!strncmp(buf,"PUT ",3+1)) {
                    method = HTTP_METHOD_PUT;
                }
                else {
                    goto error;
                };
                if (strcmp(buf+len-8,"HTTP/1.1")) { goto error;};
                buf[len-9] = 0;
                if ((rv = c->request(c, method, buf+4))) { goto error; }
            }
            else {
                /* Split header and value. */
                const char *val = "";
                for(uintptr_t i=0; i<len; i++) {
                    if (buf[i] == ':') {
                        buf[i] = 0;
                        if (len < i+2) return HTTP_STATUS_INVALID;
                        val = buf+i+2;
                        break;
                    }
                }
                if ((rv = c->header(c, buf, val))) return rv;
            }
            len = 0;
            nb_lines++;
        }
    }
    rv = 0;
  error:
    if (rv) { LOG("error = %d, len = %d\n", rv, len); }
    return rv;
}



static inline void http_write_str(struct http_req *s, const char *str) {
    s->io->write(s->io, (const uint8_t*)str, strlen(str));
}
static inline void http_write_100_resp(struct http_req *s) {
    http_write_str(s, "HTTP/1.0 100 Continue\r\n\r\n");
}

static inline void http_write_200_resp(struct http_req *s, const char *type) {
    http_write_str(s, "HTTP/1.0 200 OK\r\nContent-Type: ");
    http_write_str(s, type);
    http_write_str(s, "\r\n\r\n");
}

static inline void http_write_201_resp(struct http_req *s) {
    http_write_str(s, "HTTP/1.0 201 Created\r\n\r\n");
}


static inline void http_write_404_resp(struct http_req *s) {
    http_write_str(s, "HTTP/1.0 404 Not Found\r\nContent-Type: text/html; charset=ISO-8859-1\r\n\r\n");

}

/* Set type based on extension. */
const char *http_file_type(const char *filename) {
    const struct {
        const char *ext;
        const char *type;
    } ext_to_type[] = {
        {".c",    "text/plain; charset=UTF-8"},
        {".txt",  "text/plain; charset=UTF-8"},
        {".html", "text/html; charset=UTF-8"},
        {".js",   "text/javascript; charset=UTF-8"},
        {".wasm", "application/wasm"},
        {".svg",  "image/svg+xml"},
        {".ico",  "image/png"},
    };
    size_t n = strlen(filename);
    while(n--) {
        if (filename[n] == '.') {
            FOR_ARRAY(ext_to_type,e2t) {
                if (!strcmp(filename+n,e2t->ext)) { return e2t->type; }
            }
            break;
        }
    }
    return ext_to_type[0].type;
}

#endif
