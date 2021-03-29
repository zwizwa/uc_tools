#ifndef OS_TCP_LWIP_SOCKET_H
#define OS_TCP_LWIP_SOCKET_H


/* LwIP socket API

   This currently uses the socket wrapper, but might change to netconn
   or custom callback-to-thread wrapper.

   Note that we do not use LWIP_POSIX_SOCKETS_IO_NAMES, as that is
   implemented with macros and also renames struct members.
*/

struct os_tcp_server {
    struct sockaddr_storage addr_storage;
    int slisten;
};
#define OS_TCP_SOCKET_INIT { .socket = -1 }

static intptr_t os_tcp_server_init(struct os_tcp_server *s, uint16_t port) {
    memset(s,0,sizeof(*s));
    struct sockaddr_in *addr = (void*)&s->addr_storage;
    addr->sin_family = AF_INET;
    addr->sin_port = htons(port);

    s->slisten = lwip_socket(AF_INET, SOCK_STREAM, 0);
    LWIP_ASSERT("slisten >= 0", s->slisten >= 0);

    int ret1 = lwip_bind(s->slisten, (struct sockaddr *)addr,
                         sizeof(s->addr_storage));
    LWIP_ASSERT("ret == 0", ret1 == 0);

    int ret2 = lwip_listen(s->slisten, 0);
    LWIP_ASSERT("ret == 0", ret2 == 0);

    return 0;
}

struct os_tcp_socket {
    struct blocking_io io;
    int socket;
};

/* Move socket into different storage, e.g. for moving from transient
   stack storage to a memory pool. */
void os_tcp_socket_move(struct os_tcp_socket *dst, struct os_tcp_socket *src) {
    memcpy(dst,src,sizeof(*dst));
    memset(src,0,sizeof(*src));
}

static inline intptr_t os_tcp_read(struct os_tcp_socket *s, uint8_t *buf, uintptr_t len) {
    intptr_t have = 0;
    for(;;) {
        intptr_t rv = lwip_read(s->socket, buf, len);
        if (rv < 0) return rv;
        if (rv == 0) return -123; // FIXME, handle EOF better
        len -= rv;
        buf += rv;
        have += rv;
        if (!len) return have;
        LOG("have=%d,len=%d\n",have,len);
    }
}
static inline intptr_t os_tcp_write(struct os_tcp_socket *s, const uint8_t *buf, uintptr_t len) {
    return lwip_write(s->socket, buf, len);
}
static inline void os_tcp_done(struct os_tcp_socket *s) {
#if 0
    lwip_close(s->socket);
#else
    lwip_shutdown(s->socket, SHUT_WR);
    for (;;) {
        uint8_t buf;
        int rv = lwip_read(s->socket, &buf, 1);
        if (rv <= 0) { break; }
        // LOG("flushing %d\n", buf);
    }
    lwip_close(s->socket);
#endif
}
static inline void os_tcp_accept(struct os_tcp_server *server,
                                 struct os_tcp_socket *client) {
    struct sockaddr_storage aclient;
    socklen_t aclient_len = sizeof(aclient);
    client->io.read  = (blocking_read_fn)os_tcp_read;
    client->io.write = (blocking_write_fn)os_tcp_write;
    client->socket =
        lwip_accept(server->slisten, (struct sockaddr *)&aclient,
                    &aclient_len);
}

#endif

