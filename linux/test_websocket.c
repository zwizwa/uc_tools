#include "mod_webserver.c"

#include "assert_read.h"
#include "assert_write.h"

intptr_t read_(struct http_req *c, uint8_t *buf, uintptr_t len) {
    return assert_read(0, buf, len);
}
intptr_t write_(struct http_req *c, const uint8_t *buf, uintptr_t len) {
    assert_write(1, buf, len);
    //for(uintptr_t i=0; i<len; i++) LOG("%c", buf[i]);
    return len;
}
void close_(struct http_req *c) {
    close(1);
}
// ws = new WebSocket("ws://10.1.3.29:3456");
int main(int argc, char **argv) {
    //LOG("%s\n", __FILE__);
    if (argc == 2) {
        ASSERT_ERRNO(chdir(argv[1]));
        server_serve(read_, write_, close_);
    }
}
