#include "websocket.h"
#include "macros.h"
#include "assert_read.h"
#include "assert_write.h"

#define HTTPSERVER_MAX_HEADER_SIZE 4096

uint8_t buf[HTTPSERVER_MAX_HEADER_SIZE];

intptr_t test_read(struct http_req *c, uint8_t *buf, uintptr_t len) {
    return assert_read(0, buf, len);
}
intptr_t test_write(struct http_req *c, const uint8_t *buf, uintptr_t len) {
    assert_write(1, buf, len);
    return len;
}
intptr_t test_request(struct http_req *c, const char *uri) {
    LOG("R: %s\n", uri);
    return 0;
}
intptr_t test_header(struct http_req *c, const char *header, const char *value) {
    LOG("H: %s = %s\n", header, value);
    return 0;
}

void test_server(void) {
    struct http_req c = {
        .read = test_read,
        .write = test_write,
        .request = test_request,
        .header = test_header,
    };
    httpserver_read_headers(&c);
}

int main(int argc, char **argv) {
    LOG("%s\n", __FILE__);
    if ((argc == 2) && (!strcmp("-", argv[1]))) {
        test_server();
    }
}
