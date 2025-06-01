#ifndef MOD_WEBSERVER
#define MOD_WEBSERVER

/* This combines the protocol handlers httpserver and websocket with
   i/o handlers os_tcp and os_file.  Handling of webscoket messages is
   still abstract. */

#ifndef WEBSERVER_FILE_CHUNK
#define WEBSERVER_FILE_CHUNK 512
#endif

#ifndef WEBSERVER_FILE_NAME
#define WEBSERVER_FILE_NAME 64
#endif

/* This is the main configuration to start the web application.
   The idea is to have many web apps share the same webroot (library),
   but index.html determines how they start up. */
#ifndef WEBSERVER_INDEX_HTML
#define WEBSERVER_INDEX_HTML "index.html"
#endif

#include "os_file.h"
#include "os_thread.h"
#include "os_tcp.h"

#include "httpserver.h"
#include "websocket.h"
#include "macros.h"
#include "sha1.h"
#include "base64.h"


/* Provided elsewhere. */
ws_err_t websocket_push(struct blocking_io *io, struct ws_message *m);




const char not_found[] = "404 not found";

serve_status_t serve_get(struct webserver_req *s) {
    struct http_req *h = &s->http;
    struct os_file file;
    if (OS_OK != os_file_open(&file, s->filename, OS_FILE_READ)) {
        http_write_404_resp(h);
        if (OS_OK != os_file_open(&file, "404.html", OS_FILE_READ)) {
            http_write_str(h, not_found);
            return WEBSERVER_ERROR;
        }
        /* Fallthrough and write 404.html */
    }
    else {
        http_write_200_resp(h, http_file_type(s->filename));
    }
    uint8_t buf[WEBSERVER_FILE_CHUNK];
    for(;;) {
        /* FIXME: Handle errors better. */
        os_result_t result = os_file_read(&file, buf, sizeof(buf));
        os_error_t err = 0;
        uintptr_t nb_bytes = 0;
        if ((err = os_result_unpack(result, &nb_bytes))) break;
        if (nb_bytes == 0) break;
        if ((err = h->io->write(h->io, buf, nb_bytes))) break;
    }
    os_file_close(&file);
    return WEBSERVER_DONE;
}

serve_status_t serve_put(struct webserver_req *s) {
    struct http_req *h = &s->http;
    struct os_file file;
    if (OS_OK != os_file_open(&file, s->filename, OS_FILE_WRITE)) {
        /* FIXME: What is the proper response here? */
        LOG("Can't open %s for writing.\n", s->filename);
        return WEBSERVER_ERROR;
    }
    else {
        http_write_100_resp(h);
    }
    uint8_t buf[WEBSERVER_FILE_CHUNK];
    while(s->content_length > 0) {
        uintptr_t nb = s->content_length;
        if (nb > sizeof(buf)) { nb = sizeof(buf); }
        if (OS_OK != h->io->read(h->io, buf, nb)) break;
        if (OS_OK != os_file_write(&file, buf, nb)) break;
        s->content_length -= nb;
    }
    os_file_close(&file);
    http_write_201_resp(h);
    return WEBSERVER_DONE;
}


intptr_t is_local(const char *uri) {
    if (uri[0] == '/') uri++;
    for(const char *c = uri; *c; c++) {
        if (*c == '/')  return 0;
        if (*c == '\\') return 0;
    }
    return 1;
}

void put_request(struct http_req *c, const char *uri) {
    struct webserver_req *s = (void*)c;
    LOG("W: %s\n", uri);
    s->filename[0] = 0;
    if (uri[0] == '/') uri++;
    if (strlen(uri) < 1) return;
    s->serve = serve_put;
    int n = sizeof(s->filename);
    strncpy(s->filename, uri, n);
    s->filename[n-1] = 0;
}


serve_status_t serve_ws(struct webserver_req *s) {
    struct http_req *h = &s->http;
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
    return WEBSERVER_WEBSOCKET_UP;
}


void get_request(struct http_req *c, const char *uri) {
    struct webserver_req *s = (void*)c;
    LOG("R: %s\n", uri);
    s->filename[0] = 0;
    s->serve = (!strcmp(uri,"/ws")) ? serve_ws : serve_get;
    if (uri[0] == '/') uri++;
    if (uri[0] == 0) {
        uri = WEBSERVER_INDEX_HTML;
        LOG("-> %s\n", uri);
    }
    else {
        /* Keep it simple: all files need to be in the webroot.  No
           subdirectories allowed. */
        if (!is_local(uri)) {
            LOG("%s is not local\n", uri);
            return;
        }
    }
    int n = sizeof(s->filename);
    strncpy(s->filename, uri, n);
    s->filename[n-1] = 0;
}

http_err_t request(struct http_req *c, int method, const char *uri) {
    if (HTTP_METHOD_GET == method) {
        get_request(c, uri);
    }
    else if (HTTP_METHOD_PUT == method) {
        put_request(c, uri);
    }
    /* FIXME: Does this need error propagation? */
    return HTTP_ERR_OK;
}

http_err_t header(struct http_req *c, const char *hdr, const char *val) {
    struct webserver_req *s = (void*)c;
    // LOG("H: %s = %s\n", hdr, val);
    // FIXME: case-insensitive?
    if (!strcmp(hdr, "Sec-WebSocket-Key")) {
        ws_write_sha1(val, s->websocket_sha1);
    }
    else if (!strcmp(hdr, "Content-Length")) {
        s->content_length = strtold(val, NULL);
    }
    return HTTP_ERR_OK;
}


void server_init(struct webserver_req *s,
                 struct blocking_io *io) {
    s->http.request = request;
    s->http.header  = header;
    s->http.io      = io;
}

/* Serve a single request.  If this is a file, the function returns
   after serving a single file, closing the socket.  If it is a
   websocket, the function will keep serving until the other end
   closes. */
serve_status_t server_serve(
    struct webserver_req *s,
    struct blocking_io *io) {

    server_init(s, io);
    http_read_headers(&s->http);
    if (!s->serve) {
        LOG("no s->serve, bad request?\n");
        return WEBSERVER_ERROR;
    }
    else {
        serve_status_t status = s->serve(s);
        return status;
    }
}




OS_THREAD_STACK(ws_thread, 1024);
OS_THREAD_MAIN(ws_loop, ctx) {
    struct blocking_io *io = ctx;
    for(;;) {
        ws_err_t err = ws_read_msg(io, websocket_push);
        if (err) {
            LOG("ws_read_msg -> %d, exiting thread\n", (intptr_t)err /*FIXME*/);
            OS_THREAD_RETURN();
        }
    }
}

struct os_tcp_socket static_socket = OS_TCP_SOCKET_INIT;

void webserver_loop(uint16_t port) {
    struct os_tcp_server server;
    os_tcp_server_init(&server, port);

    // LOG("webserver_loop on port %d\n", (int)port);
    for (;;) {
        /* In our case, http request state is always transient, so it
           can go onto the stack.  The socket might be transient, or
           might be long lived in the case we receive a websocket
           protocol switch.  Initially we allocate it on the stack,
           then later it is moved to a different memory pool or static
           memory when it turns out to be a websocket.  */
        struct webserver_req req = {};
        struct os_tcp_socket socket;

        //LOG("webserver_loop accepting\n");
        os_error_t err;
        if (OS_OK != (err = os_tcp_accept(&server, &socket))) {
            /* FIXME: This happens frequently on ChibiOS. Find out why. */
            OS_LOG_ERROR("accept", err);
            continue;
        }
        //LOG("webserver_loop accepted\n");

        // client->req.http.ctx = &client->accepted.socket;

        // LOG("accept\n");
        serve_status_t status = server_serve(&req, &socket.io);

        /* Spawn a handler loop when a websocket connection was created. */
        if (WEBSERVER_WEBSOCKET_UP == status) {
            if (static_socket.io.read) {
                /* The initialized status of static_socket is used to
                   indicate that a thread is running.  We need to
                   synchronously terminate it, then restart it with
                   the new socket.  The simplest thing to do on
                   ChibiOS/LwIP for now is to close the socket, then
                   wait for the thread to terminate.  Note that in
                   LwIP this needs LWIP_NETCONN_FULLDUPLEX==1, which
                   is an experimental feature. */
                LOG("closing existing websocket, waiting for thread to terminate.\n");
                os_tcp_disconnect(&static_socket);
                OS_THREAD_WAIT(ws_thread);
                LOG("thread exited, closing socket.\n");
                os_tcp_done(&static_socket);
            }
            LOG("spawn ws_loop()\n");
            /* The socket is no longer transient, so move it.  Note
               that the protocol is no longer http -- we no longer
               need the request structs here.
               FIXME: Kill any old connections and old threads. */
            os_tcp_socket_move(&static_socket, &socket);
            static_socket.debug = 1;
            struct blocking_io *io = &static_socket.io;
            OS_THREAD_START(ws_thread, ws_loop, io);
        }
        else {
            //LOG("server_serve() -> %d\n", status);
            os_tcp_done(&socket);
            //LOG("closed\n", s);
        }
    }
}


#endif
