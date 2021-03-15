#ifndef MOD_WEBSERVER
#define MOD_WEBSERVER

/* Requirements:
   stdlib fopen/fclose */

#define WEBSERVER_FILE_CHUNK 4096
#define WEBSERVER_FILE_NAME 64

#include "websocket.h"
#include "macros.h"
#include "sha1.h"
#include "base64.h"

#define WEBSERVER_REQ_OK 0
#define WEBSERVER_REQ_ERROR -1
#define WEBSERVER_REQ_WEBSOCKET_UP 1

typedef intptr_t webserver_req_status_t;

struct webserver_req;
struct webserver_req {
    /* struct http_req is inside ws. */
    struct ws_req ws;
    webserver_req_status_t (*serve)(struct webserver_req *);
    uint8_t websocket_sha1[SHA1_BLOCK_SIZE];
    char file[WEBSERVER_FILE_NAME];
};

const char not_found[] = "404 not found";

webserver_req_status_t serve_file(struct webserver_req *s) {
    struct http_req *h = &s->ws.c;
    FILE *f = NULL;
    if (s->file[0]) {f = fopen(s->file, "r"); }
    if (!f) {
        http_write_404_resp(h);
        f = fopen("404.html", "r");
        if (!f) {
            http_write_str(h, not_found);
            return WEBSERVER_REQ_ERROR;
        }
        /* Fallthrough and write 404.html */
    }
    else {
        http_write_200_resp(h, http_file_type(s->file));
    }
    uint8_t buf[WEBSERVER_FILE_CHUNK];
    for(;;) {
        int rv = fread(buf, 1, sizeof(buf), f);
        if (rv < 1) break;
        h->write(h, buf, rv);
    }
    fclose(f);
    return WEBSERVER_REQ_OK;
}
webserver_req_status_t serve_ws(struct webserver_req *s) {
    struct http_req *h = &s->ws.c;
    http_write_str(
        h, "HTTP/1.1 101 Switching Protocols\r\n"
        "Upgrade: websocket\r\n"
        "Connection: Upgrade\r\n"
        "Sec-WebSocket-Accept: ");
    int n = base64_length(sizeof(s->websocket_sha1));
    char buf[n+1];
    base64_encode(buf, s->websocket_sha1, sizeof(s->websocket_sha1));
    buf[n] = 0;
    http_write_str(h, buf);
    http_write_str(h, "\r\n\r\n");
    /* Indicate to caller that serve_ws_msg() needs to be called in a
       loop. This gives caller the chance to spawn a thread. */
    return WEBSERVER_REQ_WEBSOCKET_UP;
}
webserver_req_status_t server_ws_loop(struct webserver_req *s, http_push push) {
    s->ws.push = push;
    for(;;) { ws_read_msg(&s->ws); }
}

intptr_t request(struct http_req *c, const char *uri) {
    struct webserver_req *s = (void*)c;
    LOG("R: %s\n", uri);
    s->file[0] = 0;
    s->serve = (!strcmp(uri,"/ws")) ? serve_ws : serve_file;
    if (uri[0] == '/') uri++;
    if (uri[0] == 0) { uri = "index.html"; }
    else {
        /* Only serve files inside the current directory. */
        for(const char *c = uri; *c; c++) {
            if (*c == '/')  return 0;
            if (*c == '\\') return 0;
        }
    }
    int n = sizeof(s->file);
    strncpy(s->file, uri, n);
    s->file[n-1] = 0;
    return 0;
}

intptr_t header(struct http_req *c, const char *hdr, const char *val) {
    struct webserver_req *s = (void*)c;
    // LOG("H: %s = %s\n", hdr, val);
    // FIXME: case-insensitive?
    if (!strcmp(hdr, "Sec-WebSocket-Key")) {
        SHA1_CTX ctx;
        sha1_init(&ctx);
        sha1_update(&ctx, (BYTE*)val, strlen(val));
        const char magic[] = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";
        sha1_update(&ctx, (BYTE*)magic, strlen(magic));
        sha1_final(&ctx, s->websocket_sha1);
    }
    return 0;
}


void server_init(struct webserver_req *s,
                 http_read  read,
                 http_write write) {
    ZERO(s);
    s->ws.c.request = request;
    s->ws.c.header  = header;
    s->ws.c.read    = read;
    s->ws.c.write   = write;
}

/* Serve a single request.  If this is a file, the function returns
   after serving a single file, closing the socket.  If it is a
   websocket, the function will keep serving until the other end
   closes. */
webserver_req_status_t server_serve(
    struct webserver_req *s,
    http_read read,
    http_write write) {

    server_init(s, read, write);
    http_read_headers(&s->ws.c);
    ASSERT(s->serve);
    webserver_req_status_t status = s->serve(s);
    return status;
}


#endif
