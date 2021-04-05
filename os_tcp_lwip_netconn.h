#ifndef OS_TCP_LWIP_NETCONN_H
#define OS_TCP_LWIP_NETCONN_H


/* ChibiOS notes.

   With BMP + ChibiOS + enabling asserts and thread fill (find out how
   exactly it works), debugger gives SIGSEGV when reading bad
   addresses e.g. 0x55555555 for uninitialized variable. */

struct os_tcp_server {
    struct netconn *netconn;
};

static os_error_t os_tcp_server_init(struct os_tcp_server *s, uint16_t port) {
    memset(s,0,sizeof(*s));
    ASSERT(s->netconn = netconn_new(NETCONN_TCP));
    ASSERT(0 == netconn_bind(s->netconn, IP_ADDR_ANY, port));
    ASSERT(0 == netconn_listen(s->netconn));
    return OS_OK;
}
struct os_tcp_socket {
    struct blocking_io io;
    struct netconn *netconn;
    struct netbuf *netbuf;
    uint32_t offset;
    uint32_t debug;
};

#define OS_TCP_SOCKET_INIT {}

/* This is used directly in streaming parsers, so make it efficient
   for small data sizes.  This blocks untill all the bytes are
   received. */

/* FIXME: This still needs merging of lwip and other errors. */
static inline const char *os_strerror(os_error_t e) {
    return lwip_strerr((intptr_t)e);
}

static inline os_error_t os_tcp_read(struct os_tcp_socket *s, uint8_t *buf, uintptr_t len) {
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
                //LOG("os_tcp_read: satisfied %d\n", nb_read);
                return OS_OK;
            }
            /* Read was not satisfied, which means the buffer was fully consumed. */
            //LOG("os_tcp_read: not satisfied nb_read=%d, left=%d\n", nb_read, len);
            netbuf_delete(s->netbuf);
            s->netbuf = NULL;
            s->offset = 0;
        }
        /* No more data, get a new netbuf. */
        if (s->debug) LOG("os_tcp_read...\n");
        int rv = netconn_recv(s->netconn, &s->netbuf);
        if (s->debug) LOG("os_tcp_read rv=%d\n", rv);
        if (rv) {
            LOG("os_tcp_read: error %d %s\n", rv, lwip_strerr(rv));
            // FIXME: this still needs merging of LwIP, FatFS and maybe ChibiOS errors
            return (os_error_t)rv;
        }
    }
}
static inline os_error_t os_tcp_write(struct os_tcp_socket *s, const uint8_t *buf, uintptr_t len) {
    // FIXME: NETCONN_NOCOPY is probably possible.
    // FIXME: map errors
    return (os_error_t)(intptr_t)netconn_write(s->netconn, buf, len, NETCONN_COPY);
}

static inline void os_tcp_socket_init(struct os_tcp_socket *s) {
    s->netconn = 0;
    s->netbuf = 0;
    s->offset = 0;
    s->io.read  = (blocking_read_fn)os_tcp_read;
    s->io.write = (blocking_write_fn)os_tcp_write;
}

static inline os_error_t os_tcp_err(intptr_t rv) {
    return (os_error_t)rv;
}

static inline os_error_t os_tcp_accept(struct os_tcp_server *server,
                                         struct os_tcp_socket *client) {
    os_tcp_socket_init(client);
    intptr_t rv = netconn_accept(server->netconn, &client->netconn);
    if (rv) {
        // LOG("os_tcp_accept: error %d %s\n", rv, lwip_strerr(rv));
    }
    return os_tcp_err(rv);
}

/* Move socket into different storage, e.g. for moving from transient
   stack storage to a memory pool. */
void os_tcp_socket_move(struct os_tcp_socket *dst, struct os_tcp_socket *src) {
    memcpy(dst,src,sizeof(*dst));
    memset(src,0,sizeof(*src));
}


/* Note that we don't use "close" to allow to distiguish disconnect
   and full resource cleanup. */

static inline void os_tcp_trace(struct os_tcp_socket *s) {
    LOG("os_tcp_trace: %p\n", s);
}

/* Close will only close the connection, but will not free any
   resources.  This can be used to close from another thread. */
static inline void os_tcp_disconnect(struct os_tcp_socket *s) {
    if (s->netconn) {
        if (s->debug) os_tcp_trace(s);
        netconn_close(s->netconn);
    }
}

/* Free all resources associated with the socket.
   This also closes the connection. */
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
