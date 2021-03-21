#ifndef MOD_WEBSERVER
#define MOD_WEBSERVER

/* This combines the protocol handlers httpserver and websocket with
   i/o handlers os_tcp and os_file.  Handling of webscoket messages is
   still abstract. */

#define WEBSERVER_FILE_CHUNK 4096
#define WEBSERVER_FILE_NAME 64

#include "os_file.h"
#include "os_thread.h"
#include "os_tcp.h"

#include "httpserver.h"
#include "websocket.h"
#include "macros.h"
#include "sha1.h"
#include "base64.h"


/* Provided elsewhere. */
ws_err_t push(struct ws_req *r, struct ws_message *m);

#define WEBSERVER_REQ_OK 0
#define WEBSERVER_REQ_ERROR -1
#define WEBSERVER_REQ_WEBSOCKET_UP 1

typedef intptr_t webserver_req_status_t;

struct webserver_req;
struct webserver_req {
    /* struct http_req is inside ws. */
    struct ws_req ws;
    webserver_req_status_t (*serve)(struct webserver_req *);
    intptr_t content_length;
    uint8_t websocket_sha1[SHA1_BLOCK_SIZE];
    char filename[WEBSERVER_FILE_NAME];
};

const char not_found[] = "404 not found";

webserver_req_status_t serve_get(struct webserver_req *s) {
    struct http_req *h = &s->ws.http;
    struct os_file file;
    if (OS_FILE_STATUS_OK !=
        os_file_open(&file, s->filename, OS_FILE_READ)) {
        http_write_404_resp(h);
        if (OS_FILE_STATUS_OK !=
            os_file_open(&file, "404.html", OS_FILE_READ)) {
            http_write_str(h, not_found);
            return WEBSERVER_REQ_ERROR;
        }
        /* Fallthrough and write 404.html */
    }
    else {
        http_write_200_resp(h, http_file_type(s->filename));
    }
    uint8_t buf[WEBSERVER_FILE_CHUNK];
    for(;;) {
        os_file_status_t rv = os_file_read(&file, buf, sizeof(buf));
        if (rv < 1) break;
        h->write(h, buf, rv);
    }
    os_file_close(&file);
    return WEBSERVER_REQ_OK;
}

webserver_req_status_t serve_put(struct webserver_req *s) {
    struct http_req *h = &s->ws.http;
    struct os_file file;
    if (OS_FILE_STATUS_OK !=
        os_file_open(&file, s->filename, OS_FILE_WRITE)) {
        /* FIXME: What is the proper response here? */
        LOG("Can't open %s for writing.\n", s->filename);
        return WEBSERVER_REQ_ERROR;
    }
    else {
        http_write_100_resp(h);
    }
    uint8_t buf[WEBSERVER_FILE_CHUNK];
    while(s->content_length > 0) {
        intptr_t rv = h->read(h, buf, sizeof(buf));
        LOG("serve_put, read %d\n", rv);
        if (rv < 1) break;
        os_file_status_t rv1 = os_file_write(&file, buf, rv);
        if (rv1 < 1) break;
        s->content_length -= rv;
    }
    os_file_close(&file);
    http_write_201_resp(h);
    return WEBSERVER_REQ_OK;
}


webserver_req_status_t serve_ws(struct webserver_req *s) {
    struct http_req *h = &s->ws.http;
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

intptr_t is_local(const char *uri) {
    if (uri[0] == '/') uri++;
    for(const char *c = uri; *c; c++) {
        if (*c == '/')  return 0;
        if (*c == '\\') return 0;
    }
    return 1;
}

intptr_t put_request(struct http_req *c, const char *uri) {
    struct webserver_req *s = (void*)c;
    LOG("W: %s\n", uri);
    s->filename[0] = 0;
    if (uri[0] == '/') uri++;
    if (strlen(uri) < 1) return 0;
    s->serve = serve_put;
    int n = sizeof(s->filename);
    strncpy(s->filename, uri, n);
    s->filename[n-1] = 0;
    return 0;
}

intptr_t get_request(struct http_req *c, const char *uri) {
    struct webserver_req *s = (void*)c;
    LOG("R: %s\n", uri);
    s->filename[0] = 0;
    s->serve = (!strcmp(uri,"/ws")) ? serve_ws : serve_get;
    if (uri[0] == '/') uri++;
    if (uri[0] == 0) { uri = "index.html"; }
    else { if (!is_local(uri)) return 0; }
    int n = sizeof(s->filename);
    strncpy(s->filename, uri, n);
    s->filename[n-1] = 0;
    return 0;
}

intptr_t request(struct http_req *c, int method, const char *uri) {
    if (HTTP_METHOD_GET == method) {
        return get_request(c, uri);
    }
    if (HTTP_METHOD_PUT == method) {
        return put_request(c, uri);
    }
    return -1; // FIXME
}

intptr_t header(struct http_req *c, const char *hdr, const char *val) {
    struct webserver_req *s = (void*)c;
    LOG("H: %s = %s\n", hdr, val);
    // FIXME: case-insensitive?
    if (!strcmp(hdr, "Sec-WebSocket-Key")) {
        SHA1_CTX ctx;
        sha1_init(&ctx);
        sha1_update(&ctx, (uint8_t*)val, strlen(val));
        const char magic[] = "258EAFA5-E914-47DA-95CA-C5AB0DC85B11";
        sha1_update(&ctx, (uint8_t*)magic, strlen(magic));
        sha1_final(&ctx, s->websocket_sha1);
    }
    else if (!strcmp(hdr, "Content-Length")) {
        s->content_length = strtold(val, NULL);
    }
    return 0;
}


void server_init(struct webserver_req *s,
                 http_read  read,
                 http_write write) {
    s->ws.http.request = request;
    s->ws.http.header  = header;
    s->ws.http.read    = read;
    s->ws.http.write   = write;
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
    http_read_headers(&s->ws.http);
    ASSERT(s->serve);
    webserver_req_status_t status = s->serve(s);
    return status;
}




struct client {
    struct webserver_req req;
    struct os_tcp_accepted a;
};
intptr_t http_read_(struct http_req *r, uint8_t *buf, uintptr_t len) {
    return os_tcp_read(r->ctx, buf, len);
}
intptr_t http_write_(struct http_req *r, const uint8_t *buf, uintptr_t len) {
    return os_tcp_write(r->ctx, buf, len);
}

OS_THREAD_STACK(ws_thread, 1024);
OS_THREAD_MAIN(ws_loop, ctx) {
    struct client *c = ctx;
    server_ws_loop(&c->req, push);
    OS_THREAD_RETURN();
}

void webserver_loop(uint16_t port) {
    struct os_tcp_server s;
    os_tcp_server_init(&s, port);

    for (;;) {
        printf("accepting\n");
        struct client *c = calloc(1, sizeof(*c));
        os_tcp_accept(&s, &c->a);
        c->req.ws.http.ctx = &c->a.socket;

        // LOG("accept\n");
        webserver_req_status_t s = server_serve(&c->req, http_read_, http_write_);

        /* Spawn a handler loop when a websocket connection was created. */
        if (WEBSERVER_REQ_WEBSOCKET_UP == s) {
            LOG("spawn ws_loop()\n");
            OS_THREAD_START(ws_thread, ws_loop, c);
        }
        else {
            LOG("server_serve() -> %d\n", s);
            os_tcp_close(&c->a.socket);
            //LOG("closed\n", s);
            free(c);
        }
    }
}





#endif
