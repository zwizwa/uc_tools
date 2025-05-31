#ifndef HTTPSERVER_H
#define HTTPSERVER_H

/* Minimalistic HTTP server implementation.
   This is intended to serve some simple pages and to bootstrap a websocket.

   Temporary buffers are allocated on the stack, and data is passed to
   a callback in parsed form.  ( Fold instead of data. )
*/

#define HTTP_READ  BLOCKING_IO_READ
#define HTTP_WRITE BLOCKING_IO_WRITE

#include "macros.h"

#include "sha1.h"
#include "base64.h"
#include "blocking_io.h"


/* The http errors are a combination of os errors and application
   errors.  We still use the signed integer encoding cast to a
   pointer to be able to distinguish different types properly. */
struct http_err {};
typedef const struct http_err *http_err_t;

struct blocking_io;
struct http_req;

struct http_req {
    http_err_t (*request)(struct http_req *, int method, const char *uri);
    http_err_t (*header)(struct http_req *, const char *header, const char *value);
    /* The i/o is abstract.  This is done essentially for websockets,
       which only need socket state.  This way the socket can be
       allocated separately in a memory pool, and transient http state
       can be kept on the stack. */
    struct blocking_io *io;
};

/* Error propagation.

   We don't need to know a whole lot at the toplevel, so we will not
   propagate low level errors, just log them.

   Errors are encoded as pointers to allow for type checking.
*/

struct webserver_status {};
typedef const struct webserver_status *serve_status_t;

#define WEBSERVER_STATUS(n)  ((serve_status_t)(0x2000 + ((n)&0xF)))
#define WEBSERVER_DONE         WEBSERVER_STATUS(0)
#define WEBSERVER_ERROR        WEBSERVER_STATUS(1)
#define WEBSERVER_WEBSOCKET_UP WEBSERVER_STATUS(2)

#ifndef WEBSERVER_FILE_NAME
#define WEBSERVER_FILE_NAME 64
#endif

struct webserver_req;
struct webserver_req {
    /* struct http_req is inside ws. */
    struct http_req http;
    serve_status_t (*serve)(struct webserver_req *);
    intptr_t content_length;
    int cont; // what to do after serving http request
    uint8_t websocket_sha1[SHA1_BLOCK_SIZE];
    char filename[WEBSERVER_FILE_NAME];
};


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


/* Wrapper for os errors. */
static inline http_err_t http_err_os(os_error_t e) {
    return (http_err_t)e;
}
/* The other errors are part of this enumeration. */
#define HTTP_ERR_BASE (-0x1000) /* for remapping if necessary. */
#define HTTP_ERR(n) ((http_err_t)(((n)&0xF) + HTTP_ERR_BASE))

#define HTTP_ERR_OVERRUN HTTP_ERR(1)
#define HTTP_ERR_INVALID HTTP_ERR(2)

#define HTTP_ERR_OK http_err_os(OS_OK)


struct http_req;
typedef void     (*http_close)(struct http_req *);




void log_c(const uint8_t *buf, uintptr_t len) {
    for(uintptr_t i=0; i<len; i++) LOG("%c", buf[i]);
}

/* FIXME: These share the OS address space.  Provide an explicit mapper. */

#define HTTP_METHOD_GET 1
#define HTTP_METHOD_PUT 2


/* Don't do anything fancy, just read the headers into a buffer. */
static inline http_err_t http_read_headers(struct http_req *c) {
    /* We parse one line at a time, which allows us to keep the buffer
       small, and locally managed.  Callbacks can use the memory in
       the extent of the call. */
    os_error_t error = OS_OK;
    char buf[4096];
    uintptr_t len = 0;
    uintptr_t nb_lines = 0;
    for(;;) {
        if (len >= sizeof(buf)) return HTTP_ERR_OVERRUN;
        HTTP_READ(c->io, (uint8_t*)&buf[len++], 1);
        if ((len >= 2) && (buf[len-2] == 0xd) && (buf[len-1] == 0xa)) {
            /* End of line. Convert 0d,0a to cstring. */
            len -= 2; buf[len] = 0;
            if (len == 0) { break; }
            if (0 == nb_lines) {
                /* Only HTTP/1.1 GET is supported. */
                if (len < (3+1+1+1+8)) {
                    return HTTP_ERR_INVALID;
                };
                int method;
                if (!strncmp(buf,"GET ",3+1)) {
                    method = HTTP_METHOD_GET;
                }
                else if (!strncmp(buf,"PUT ",3+1)) {
                    method = HTTP_METHOD_PUT;
                }
                else {
                    return HTTP_ERR_INVALID;
                };
                if (strcmp(buf+len-8,"HTTP/1.1")) {
                    return HTTP_ERR_INVALID;
                };
                buf[len-9] = 0;
                http_err_t req_err;
                if (HTTP_ERR_OK != (req_err = c->request(c, method, buf+4))) {
                    return req_err;
                }
            }
            else {
                /* Split header and value. */
                const char *val = "";
                for(uintptr_t i=0; i<len; i++) {
                    if (buf[i] == ':') {
                        buf[i] = 0;
                        if (len < i+2) {
                            return HTTP_ERR_INVALID;
                        }
                        val = buf+i+2;
                        break;
                    }
                }
                http_err_t req_err;
                if ((req_err = c->header(c, buf, val))) {
                    return req_err;
                }
            }
            len = 0;
            nb_lines++;
        }
    }
    return HTTP_ERR_OK;
  error_exit:
    /* os error handler. */
    OS_LOG_ERROR("http_read_headers", error);
    return http_err_os(error);
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
