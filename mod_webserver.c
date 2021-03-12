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

struct server_req;
struct server_req {
    /* struct http_req is inside ws. */
    struct ws_req ws;
    void (*serve)(struct server_req *);
    uint8_t websocket_sha1[SHA1_BLOCK_SIZE];
    char file[WEBSERVER_FILE_NAME];
};

const char not_found[] = "404 not found";

void serve_file(struct server_req *s) {
    struct http_req *h = &s->ws.c;
    FILE *f = NULL;
    if (s->file[0]) {f = fopen(s->file, "r"); }
    if (!f) {
        http_write_404_resp(h);
        f = fopen("404.html", "r");
        if (!f) {
            http_write_str(h, not_found);
            return;
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
}
void serve_ws(struct server_req *s) {
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
    for(;;) { ws_read_msg(&s->ws); }
}

void test_reply(struct ws_req *r, uint8_t *buf, uintptr_t len) {
    //LOG("push: sending reply\n");
    struct ws_message m = {
        .opcode = 2,  // binary
        .fin = 1,
        .mask = 0,
        .buf = buf,
        .len = len
    };
    ws_write_msg(r, &m);
}

#define TEST_REPLY(val, ...)                      \
    if (!strcmp((const char*)m->buf, val)) {      \
        uint8_t buf[] = {__VA_ARGS__};            \
        test_reply(r, buf, sizeof(buf));          \
    }

ws_err_t push(struct ws_req *r, struct ws_message *m) {
    //LOG("m.len = %d\n", m->len);
    m->buf[m->len] = 0; // FIXME: don't do this
    LOG("push: %s\n", m->buf);
    TEST_REPLY("1", 1, 42); // T_INT, 42
    TEST_REPLY("2", 2, 0);  // T_TUP, 0
    return 0;
}

intptr_t request(struct http_req *c, const char *uri) {
    struct server_req *s = (void*)c;
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
    struct server_req *s = (void*)c;
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

void server_init(struct server_req *s,
                 http_read  read,
                 http_write write,
                 http_close close) {
    ZERO(s);
    s->ws.c.request = request;
    s->ws.c.header  = header;
    s->ws.push      = push;
    s->ws.c.read    = read;
    s->ws.c.write   = write;
}

/* Serve a single request.  If this is a file, the function returns
   after serving a single file, closing the socket.  If it is a
   websocket, the function will keep serving until the other end
   closes. */
void server_serve(http_read read, http_write write, http_close close) {
    struct server_req s;
    server_init(&s, read, write, close);
    http_read_headers(&s.ws.c);
    ASSERT(s.serve);
    s.serve(&s);
    if (s.ws.c.close) {
        s.ws.c.close(&s.ws.c);
    }
}


#endif
