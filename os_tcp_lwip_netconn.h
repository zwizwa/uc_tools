#ifndef OS_TCP_LWIP_NETCONN_H
#define OS_TCP_LWIP_NETCONN_H

struct os_tcp_server {
    struct sockaddr_storage addr_storage;
    struct netconn *netconn;
};

static intptr_t os_tcp_server_init(struct os_tcp_server *s, uint16_t port) {
    memset(s,0,sizeof(*s));
    ASSERT(s->netconn = netconn_new(NETCONN_TCP));
    ASSERT(0 == netconn_bind(s->netconn, IP_ADDR_ANY, port));
    ASSERT(0 == netconn_listen(s->netconn));
    return 0;
}
struct os_tcp_socket {
    struct blocking_io io;
    struct netconn *netconn;
    struct netbuf *netbuf;
};
#define OS_TCP_SOCKET_INIT {}

static inline intptr_t os_tcp_read(struct os_tcp_socket *s, uint8_t *buf, uintptr_t len) {
    (void)buf;
    (void)len;
    while(len > 0) {
        /* If we still have a netbuf, empty it first. */
        /* Get a new netbuf. */
        ASSERT(0 == netconn_recv(s->netconn, &s->netbuf));
    }
    return -1;
}
static inline intptr_t os_tcp_write(struct os_tcp_socket *s, const uint8_t *buf, uintptr_t len) {
    // FIXME: NETCONN_NOCOPY is probably possible.
    ASSERT(0 == netconn_write (s->netconn, buf, len, NETCONN_COPY));
    return len;
}
static inline void os_tcp_accept(struct os_tcp_server *server,
                                 struct os_tcp_socket *client) {
    client->io.read  = (blocking_read_fn)os_tcp_read;
    client->io.write = (blocking_write_fn)os_tcp_write;
    ASSERT(0 == netconn_accept(server->netconn, &client->netconn));
}

/* Move socket into different storage, e.g. for moving from transient
   stack storage to a memory pool. */
void os_tcp_socket_move(struct os_tcp_socket *dst, struct os_tcp_socket *src) {
    memcpy(dst,src,sizeof(*dst));
    memset(src,0,sizeof(*src));
}
static inline void os_tcp_done(struct os_tcp_socket *s) {
    // FIXME: free buffer
    (void)s->netbuf;
    netconn_delete(s->netconn);
}

#endif
