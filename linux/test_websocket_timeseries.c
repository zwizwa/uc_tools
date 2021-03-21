#define MMAP_FILE_LOG LOG

#include "mod_minmax.c"

#include "mod_webserver.c"

#include "assert_read.h"
#include "assert_write.h"

#include "tcp_tools.h"

#include "leb128s.h"

#include "os_thread.h"

/* The name of the map refers to the handler function. */
#define DEF_MAP DEF_TAG_U32_CONST_MAP_HANDLE

struct minmax_map    map;
struct minmax_cursor cursor;

int zoom(struct tag_u32 *req) {
    TAG_U32_UNPACK(req, 0, m, win_w, win_h, win_x, level_inc) {
        ASSERT(m->win_w < 10000); // bug guard
        ASSERT(m->win_x < m->win_w);
        struct minmax_minmax buf[m->win_w];
        int16_t new_level = minmax_cursor_zoom(
            &cursor, &map,
            buf, m->win_w, m->win_h, m->win_x, m->level_inc);
        (void)new_level;
        MINMAX_LOG("new_level = %d\n", new_level);
        send_reply_tag_u32_status(req, 0, (const uint8_t*)buf, sizeof(buf));
        return 0;
    }
    return -1;
}

DEF_MAP(
    map_cmd,
    {"zoom", "cmd", zoom, 4}
    )

DEF_MAP(
    map_root,
    {"window", "map", map_cmd},
    )

/* Protocol handler entry point. */
int handle_tag_u32(struct tag_u32 *req) {
    int rv = map_root(req);
    if (rv) {
        /* Always send a reply when there is a from address. */
        LOG("map_root() returned %d\n", rv);
        send_reply_tag_u32_status_cstring(req, 1, "bad_ref");
    }
    return 0;
}



struct client {
    struct webserver_req req;
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
    os_thread_exit(NULL);
}
intptr_t write_(struct http_req *r, const uint8_t *buf, uintptr_t len) {
    struct client *c = (void*)r;
    assert_write(c->socket, buf, len);
    //for(uintptr_t i=0; i<len; i++) LOG("%c", buf[i]);
    return len;
}

/* FIXME: Mutual exclusion access is not implemented in this test.  Do
 * not use two websockets at the same time. */


static inline void log_u32(const char *tag, uint32_t nb, const uint32_t *arr) {
    LOG("%s",tag);
    for(int32_t i=0; i<nb; i++) { LOG(" %d", arr[i]); }
    LOG("\n");
}
static inline void log_hex(const char *tag, uint32_t nb, const uint8_t *buf) {
    LOG("%s",tag);
    for(int32_t i=0; i<nb; i++) { LOG(" %02x", buf[i]); }
    LOG("\n");
}

/* Return path. */
struct msg_ctx {
    struct tag_u32 msg;
};
void reply_tag_u32(const struct tag_u32 *req, const struct tag_u32 *rpl) {
    struct client *c = req->reply_ctx;

    uint8_t buf[1024*16]; // FIXME
    struct leb128s s = {
        .buf = buf,
        .len = sizeof(buf)
    };
    leb128s_write_i32(&s, T_TAG);              if(s.error) goto error;
    leb128s_write_tag_u32_reply(&s, req, rpl); if(s.error) goto error;
    //log_hex("enc: ", s.offset, buf);

    struct ws_message m = {
        .opcode = 2,  // binary
        .fin = 1,
        .mask = 0,
        .buf = buf,
        .len = s.offset,
    };
    ws_write_msg(&c->req.ws, &m);
    return;
  error:
    LOG("leb128 write error %x\n", s.error);
    return;
}

/* Incoming request from websocket. */
leb128s_status_t push_tag_u32(struct leb128s *s, struct tag_u32 *msg) {
    msg->reply = reply_tag_u32;
    msg->reply_ctx = s->env->ctx;
    if (0) {
        log_u32("from: ", msg->nb_from, msg->from);
        log_u32("to:   ", msg->nb_args, msg->args);
        log_hex("bin:  ", msg->nb_bytes, msg->bytes);
    }
    handle_tag_u32(msg);
    return 0;
}

ws_err_t push(struct ws_req *r, struct ws_message *m) {
    struct client *c = (void*)r;

    // m->buf[m->len] = 0; // FIXME: don't do this
    // LOG("push: %s\n", m->buf);
    if (0) {
        LOG("ws_push:");
        for(int i=0;i<m->len;i++) LOG(" %02x", m->buf[i]);
        LOG("\n");
    }

    struct leb128s_env env = {
        .tag_u32 = push_tag_u32,
        .ctx = c,
    };

    struct leb128s s = { .buf = m->buf, .len = m->len, .env = &env };

    /* This calls the push_tag_u32 callback when a T_TAG message is
       received. */
    leb128_id_t id = leb128s_element(&s);

    (void)id;
    if(s.error) {
        LOG("error %d\n", s.error);
    }
    return 0;
}

OS_THREAD_STACK(ws_thread, 1024);
OS_THREAD_MAIN(ws_loop, ctx) {
    struct client *c = ctx;
    server_ws_loop(&c->req, push);
    OS_THREAD_RETURN();
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

    minmax_open(&map, argv[2], 8);

    int server_fd = assert_tcp_listen(3456);
    for(;;) {
        struct client *c = calloc(1, sizeof(*c));
        c->socket = assert_accept(server_fd);
        // LOG("accept\n");
        webserver_req_status_t s =
            server_serve(&c->req, read_, write_);

        /* Spawn a handler loop when a websocket connection was created. */
        if (WEBSERVER_REQ_WEBSOCKET_UP == s) {
            LOG("spawn ws_loop()\n");
            OS_THREAD_START(ws_thread, ws_loop, c);
        }
        else {
            LOG("server_serve() -> %d\n", s);
            /* Close the TCP socket. */
            shutdown(c->socket, SHUT_WR);
            for (;;) {
                uint8_t buf;
                int rv = read(c->socket, &buf, 1);
                if (rv <= 0) { break; }
                // LOG("flushing %d\n", buf);
            }
            close(c->socket);
            //LOG("closed\n", s);
            free(c);
        }
    }
}

