#include "websocket.h"
#include "macros.h"
#include "assert_read.h"
#include "assert_write.h"

#define HTTPSERVER_MAX_HEADER_SIZE 4096

uint8_t buf[HTTPSERVER_MAX_HEADER_SIZE];

intptr_t read_(struct http_req *c, uint8_t *buf, uintptr_t len) {
    return assert_read(0, buf, len);
}
intptr_t write_(struct http_req *c, const uint8_t *buf, uintptr_t len) {
    assert_write(1, buf, len);
    return len;
}

struct server;
struct server {
    struct http_req c;
    void (*handler)(struct server *);
};

const char root[] = "hello";
const char not_found[] = "404 not found";

void serve_html(struct server *s, const char *resp, const char *html) {
    write_(&s->c, (const uint8_t*)resp, strlen(resp));
    write_(&s->c, (const uint8_t*)html, strlen(html));
}
void handle_root(struct server *s) {
    return serve_html(s, "HTTP/1.0 200 OK\r\nContent-Type: text/html; charset=ISO-8859-1\r\n\r\n", root);
}
void handle_404(struct server *s) {
    return serve_html(s, "HTTP/1.0 404 Not Found\r\nContent-Type: text/html; charset=ISO-8859-1\r\n\r\n", not_found);
}
void handle_ws(struct server *s) {
    // TODO: send response
    // switch to websocket main loop
}


intptr_t request(struct http_req *c, const char *uri) {
    struct server *s = (void*)c;
    LOG("R: %s\n", uri);
    if (!strcmp(uri,"/"))   { s->handler = handle_root; return 0; }
    if (!strcmp(uri,"/ws")) { s->handler = handle_ws;   return 0; }
    s->handler = handle_404;
    return 0;
}
intptr_t header(struct http_req *c, const char *hdr, const char *val) {
    LOG("H: %s = %s\n", hdr, val);
    return 0;
}

void test_server(void) {
    struct server s = {
        .c = {
            .read    = read_,
            .write   = write_,
            .request = request,
            .header  = header,
        },
    };
    httpserver_read_headers(&s.c);
    if (s.handler) s.handler(&s);
}

// ws = new WebSocket("ws://10.1.3.29:3456");
int main(int argc, char **argv) {
    LOG("%s\n", __FILE__);
    if ((argc == 2) && (!strcmp("-", argv[1]))) {
        test_server();
    }
}
