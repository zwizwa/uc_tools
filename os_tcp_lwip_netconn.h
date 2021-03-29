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
    uint32_t offset;
};
#define OS_TCP_SOCKET_INIT {}

/* This is used directly in streaming parsers, so make it efficient
   for small data sizes.  This blocks untill all the bytes are
   received. */
static inline intptr_t os_tcp_read(struct os_tcp_socket *s, uint8_t *buf, uintptr_t len) {
    intptr_t nb_read = 0;
    for (;;) {
        if (s->netbuf) {
            uint16_t chunk = pbuf_copy_partial(s->netbuf->p, buf, len, s->offset);
            nb_read   += chunk;
            s->offset += chunk;
            buf       += chunk;
            len       -= chunk;
            if (len == 0) {
                /* Read was satisfied. */
                return nb_read;
            }
            /* Read was not satisfied, which means the buffer was fully consumed. */
            netbuf_delete(s->netbuf);
            s->netbuf = NULL;
        }
        /* No more data, get a new netbuf. */
        int rv = netconn_recv(s->netconn, &s->netbuf);
        if (rv) {
            LOG("netconn_recv rv = %d %s\n", rv, lwip_strerr(rv));
            return rv;
        }
    }
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
    if (s->netbuf) {
        netbuf_delete(s->netbuf);
        s->netbuf = 0;
    }
    if (s->netconn) {
        netconn_delete(s->netconn);
        s->netconn = 0;
    }
}

#endif
