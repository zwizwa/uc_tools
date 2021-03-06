#ifndef HTTPSERVER_H
#define HTTPSERVER_H

/* Minimalistic HTTP server implementation.
   This is intended to serve some simple pages and to bootstrap a websocket.

   Temporary buffers are allocated on the stack, and data is passed to
   a callback in parsed form.  ( Fold instead of data. )
*/

typedef intptr_t http_err_t;

struct http_req;
typedef intptr_t (*http_read)(struct http_req *, uint8_t *buf, uintptr_t len);
typedef intptr_t (*http_write)(struct http_req *, const uint8_t *buf, uintptr_t len);
typedef void     (*http_close)(struct http_req *);

struct http_req {
    http_read  read;
    http_write write;
    http_close close;
    http_err_t (*request)(struct http_req *, const char *uri);
    http_err_t (*header)(struct http_req *, const char *header, const char *value);
};


void log_c(const uint8_t *buf, uintptr_t len) {
    for(uintptr_t i=0; i<len; i++) LOG("%c", buf[i]);
}

#define HTTP_STATUS_EOF            -1
#define HTTP_STATUS_OVERRUN        -2
#define HTTP_STATUS_INVALID        -3

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
        rv = c->read(c, (uint8_t*)&buf[len++], 1);
        if (rv == 0) return HTTP_STATUS_EOF;
        ASSERT(1 == rv);
        if ((len >= 2) && (buf[len-2] == 0xd) && (buf[len-1] == 0xa)) {
            /* Convert 0d,0a to cstring. */
            len -= 2; buf[len] = 0;
            if (len == 0) { break; }
            if (0 == nb_lines) {
                /* Only HTTP/1.1 GET is supported. */
                rv = HTTP_STATUS_INVALID;
                if (len < (3+1+1+1+8)) { goto error;};
                if (strncmp(buf,"GET ",3+1)) { goto error;};
                if (strcmp(buf+len-8,"HTTP/1.1")) { goto error;};
                buf[len-9] = 0;
                if ((rv = c->request(c, buf+4))) { goto error; }
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
    s->write(s, (const uint8_t*)str, strlen(str));
}
static inline void http_write_200_resp(struct http_req *s, const char *type) {
    http_write_str(s, "HTTP/1.0 200 OK\r\nContent-Type: ");
    http_write_str(s, type);
    http_write_str(s, "\r\n\r\n");
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
        {".c","text/plain; charset=UTF-8"},
        {".txt","text/plain; charset=UTF-8"},
        {".html","text/html; charset=UTF-8"},
        {".js",  "text/javascript; charset=UTF-8"},
        {".wasm","application/wasm"},
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
