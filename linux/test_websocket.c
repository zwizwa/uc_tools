#include "websocket.h"
#include "macros.h"
#include "assert_read.h"
#include "assert_write.h"
#include "sha1.h"
#include "base64.h"

intptr_t read_(struct http_req *c, uint8_t *buf, uintptr_t len) {
    return assert_read(0, buf, len);
}
intptr_t write_(struct http_req *c, const uint8_t *buf, uintptr_t len) {
    assert_write(1, buf, len);
    for(uintptr_t i=0; i<len; i++) LOG("%c", buf[i]);
    return len;
}

struct server;
struct server {
    struct ws_req ws;
    void (*serve)(struct server *);
    uint8_t websocket_sha1[SHA1_BLOCK_SIZE];
    const char *resp;
    const char *file;
};

const char root[] = "hello";
const char not_found[] = "404 not found";

void write_str(struct server *s, const char *str) {
    write_(&s->ws.c, (const uint8_t*)str, strlen(str));
}
void serve_html(struct server *s, const char *resp, const char *html) {
    write_str(s, resp);
    write_str(s, html);
}

const char *resp_html = "HTTP/1.0 200 OK\r\nContent-Type: text/html; charset=UTF-8\r\n\r\n";
const char *resp_js   = "HTTP/1.0 200 OK\r\nContent-Type: text/javascript; charset=UTF-8\r\n\r\n";

void serve_file(struct server *s) {
    ASSERT(s->resp);
    ASSERT(s->file);
    write_str(s, s->resp);
    FILE *f = fopen(s->file, "r");
    ASSERT(f);
    uint8_t buf[4096];
    for(;;) {
        int rv = fread(buf, 1, sizeof(buf), f);
        if (rv < 1) break;
        write_(&s->ws.c, buf, rv);
    }
    fclose(f);
}
void serve_404(struct server *s) {
    write_str(s, "HTTP/1.0 404 Not Found\r\nContent-Type: "
              "text/html; charset=ISO-8859-1\r\n\r\n");
    write_str(s, not_found);
}
void serve_ws(struct server *s) {
    write_str(s, "HTTP/1.1 101 Switching Protocols\r\n"
              "Upgrade: websocket\r\n"
              "Connection: Upgrade\r\n"
              "Sec-WebSocket-Accept: ");
    int n = base64_length(sizeof(s->websocket_sha1));
    char buf[n+1];
    base64_encode(buf, s->websocket_sha1, sizeof(s->websocket_sha1));
    buf[n] = 0;
    write_str(s, buf);
    write_str(s, "\r\n\r\n");
    for(;;) { ws_read_msg(&s->ws); }
}

ws_err_t push(struct ws_req *r, struct ws_message *m) {
    //LOG("m.len = %d\n", m->len);
    m->buf[m->len] = 0; // FIXME: don't do this
    LOG("push: %s\n", m->buf);
    if (!strcmp((const char*)m->buf, "123")) {
        uint8_t buf[3] = {'a','b','c'};
        LOG("push: sending reply\n");
        struct ws_message m = {
            .opcode = 1,
            .fin = 1,
            .mask = 0,
            .buf = buf,
            .len = sizeof(buf)
        };
        ws_write_msg(r, &m);
    }
    return 0;
}

intptr_t request(struct http_req *c, const char *uri) {
    struct server *s = (void*)c;
    LOG("R: %s\n", uri);
    if (!strcmp(uri,"/"))      { s->serve = serve_file; s->resp = resp_html, s->file = "index.html"; return 0; }
    if (!strcmp(uri,"/lib.js")){ s->serve = serve_file; s->resp = resp_js,   s->file = "lib.js"; return 0; }
    if (!strcmp(uri,"/ws"))    { s->serve = serve_ws; return 0; }
    s->serve = serve_404;
    return 0;
}
intptr_t header(struct http_req *c, const char *hdr, const char *val) {
    struct server *s = (void*)c;
    LOG("H: %s = %s\n", hdr, val);
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

void test_server(void) {
    struct server s = {
        .ws = {
            .c = {
                .read    = read_,
                .write   = write_,
                .request = request,
                .header  = header,
            },
            .push = push
        },
    };
    httpserver_read_headers(&s.ws.c);
    ASSERT(s.serve);
    s.serve(&s);
}

// ws = new WebSocket("ws://10.1.3.29:3456");
int main(int argc, char **argv) {
    LOG("%s\n", __FILE__);
    if (argc == 2) {
        ASSERT_ERRNO(chdir(argv[1]));
        test_server();
    }
}
