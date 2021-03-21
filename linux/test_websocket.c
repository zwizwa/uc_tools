#include "mod_webserver.c"

#include "assert_read.h"
#include "assert_write.h"

intptr_t read_(struct blocking_io *c, uint8_t *buf, uintptr_t len) {
    return assert_read(0, buf, len);
}
intptr_t write_(struct blocking_io *c, const uint8_t *buf, uintptr_t len) {
    assert_write(1, buf, len);
    //for(uintptr_t i=0; i<len; i++) LOG("%c", buf[i]);
    return len;
}

void test_reply(struct ws_req *r, uint8_t *buf, uintptr_t len) {
    LOG("push: sending %d bytes\n", len);
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
    TEST_REPLY("1", 1, 42); // T_INT
    TEST_REPLY("2", 2, 1, 3, 123, 56, 1);  // T_TUP
    TEST_REPLY("3", 3, 4, 1,2,3,4);  // T_BIN
    // This is a full TAG RPC message to the wave viewer
    TEST_REPLY("7",
               7,
               0, // from
               3, 0,0,0, // to
               4, 0,0,100,0) // bin
    return 0;
}


// ws = new WebSocket("ws://10.1.3.29:3456");
int main(int argc, char **argv) {
    //LOG("%s\n", __FILE__);
    struct webserver_req s;
    if (argc == 2) {
        ASSERT_ERRNO(chdir(argv[1]));
        if (WEBSERVER_REQ_WEBSOCKET_UP ==
            server_serve(&s, read_, write_)) {
            server_ws_loop(&s, push);
        }
    }
}
