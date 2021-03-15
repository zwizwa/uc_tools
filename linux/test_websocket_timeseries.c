#define MMAP_FILE_LOG LOG

#include "mod_minmax.c"
#if 0
/* tag_u32 on stdio */
#include "mod_tag_u32_stream.c"
#include "mod_send_tag_u32.c"
#else
/* tag_u32 / leb128 on websocket */
#endif

#include "mod_webserver.c"

#include "assert_read.h"
#include "assert_write.h"

#include "tcp_tools.h"

#include <pthread.h>

/* The name of the map refers to the handler function. */
#define DEF_MAP DEF_TAG_U32_CONST_MAP_HANDLE

struct minmax_map    map;
struct minmax_cursor cursor;



struct client {
    struct webserver_req req;
    pthread_t thread;
    int socket;
};
intptr_t read_(struct http_req *r, uint8_t *buf, uintptr_t len) {
    struct client *c = (void*)r;
    LOG("read...\r");
    // ssize_t rv = assert_read(c->socket, buf, len);
    // FIXME: This can produce a short read.
    ssize_t rv = read(c->socket, buf, len);
    if (rv > 0) {
        LOG("read ok\r");
        return rv;
    }
    ASSERT_ERRNO(rv);
    LOG("read EOF, exit thread\n");
    // Any error terminates the thread
    // free(c);  // not sure if this is ok before exit.. just leak it.
    pthread_exit(NULL);
}
intptr_t write_(struct http_req *r, const uint8_t *buf, uintptr_t len) {
    struct client *c = (void*)r;
    assert_write(c->socket, buf, len);
    //for(uintptr_t i=0; i<len; i++) LOG("%c", buf[i]);
    return len;
}

/* FIXME: Mutual exclusion access is not implemented in this test.  Do
 * not use two websockets at the same time. */

/* Incoming request from websocket. */
ws_err_t push(struct ws_req *r, struct ws_message *m) {
    // m->buf[m->len] = 0; // FIXME: don't do this
    // LOG("push: %s\n", m->buf);
    LOG("push:");
    for(int i=0;i<m->len;i++) LOG(" %02x", m->buf[i]);
    LOG("\n");

#if 0
    /* We don't know how many tags there are, so just guess a bunch.
     * Later abstract this a bit. */
    uint32_t max_nb_tags = 16;
    uint32_t nb_tags = 0;
    uint32_t tags[16];
    uint32_t n = 0;
    uint32_t n1 = leb128_read(buf+n, m->len-n, tags + nb_tags);
#endif

    return 0;
}
void *ws_loop(void *ctx) {
    struct client *c = ctx;
    //for (;;) sleep(1);
    server_ws_loop(&c->req, push);
    return NULL;
}

// ws = new WebSocket("ws://10.1.3.29:3456");
// see test_websocket_timeseries.sh
int main(int argc, char **argv) {

    /* Note that this is different from test_websocket.c, which has a
       main() that only handles a single request and needs the
       test_websocket.sh wrapper to make a TCP server.

       Here we spawn a full webserver because the minmax state is
       shared, so we only initialize it once. */

    if (argc != 3) {
        LOG("usage: %s <documentroot> <test.raw>\n", argv[0]);
        return 1;
    }
    ASSERT_ERRNO(chdir(argv[1]));

    //minmax_open(&map, argv[2], 8);

    int server_fd = assert_tcp_listen(3456);
    for(;;) {
        struct client *c = calloc(1, sizeof(*c));
        c->socket = assert_accept(server_fd);
        // LOG("accept\n");
        webserver_req_status_t s =
            server_serve(&c->req, read_, write_);

        /* Spawn a handler loop when a websocket connection was created. */
        if (WEBSERVER_REQ_WEBSOCKET_UP == s) {
            LOG("spawn websocket handler\n");
            pthread_create(&c->thread, NULL, ws_loop, c);
        }
        else {
            //LOG("s = %d\n", s);
            /* Close the TCP socket. */
            shutdown(c->socket, SHUT_WR);
            for (;;) {
                uint8_t buf;
                int rv = read(c->socket, &buf, 1);
                if (rv <= 0) { break; }
                LOG("flushing %d\n", buf);
            }
            close(c->socket);
            //LOG("closed\n", s);
            free(c);
        }
    }
}

