#ifndef OS_TCP_H
#define OS_TCP_H

/* Instead of abstracting at the socket api, abstract at a highe
   rlevel and provide a simplified view of a TCP server. */

/* Note that this does not assume that files and sockets have the same
   read/write API. */

/* LwIP socket API

   Note that we do not use LWIP_POSIX_SOCKETS_IO_NAMES, as that is
   implemented with macros and also renames struct members.
*/
#ifdef LWIP_HDR_SOCKETS_H

struct os_tcp_server {
    struct sockaddr_storage addr_storage;
    int slisten;
};

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
    int socket;
};
static inline void os_tcp_write(struct os_tcp_socket *s, const uint8_t *buf, uintptr_t len) {
    lwip_write(s->socket, buf, len);
}
static inline void os_tcp_close(struct os_tcp_socket *s) {
    lwip_close(s->socket);
}
struct os_tcp_accepted {
    struct os_tcp_socket socket;
    struct sockaddr_storage client;
};
static inline void os_tcp_accept(struct os_tcp_server *s, 
                                 struct os_tcp_accepted *a) {
    socklen_t aclient_len = sizeof(&a->client);
    a->socket.socket =
        lwip_accept(s->slisten, (struct sockaddr *)&a->client,
                    &aclient_len);
}

/* Berkeley sockets */
#else
#include "tcp_tools.h"

struct os_tcp_socket {
    int fd;
};
struct os_tcp_server {
    int fd;
};
struct os_tcp_accepted {
    struct os_tcp_socket socket;
};
static intptr_t os_tcp_server_init(struct os_tcp_server *s, uint16_t port) {
    s->fd = assert_tcp_listen(3456);
    return 0;
}
static inline void os_tcp_accept(struct os_tcp_server *serv,
                                 struct os_tcp_accepted *a) {
    memset(a,0,sizeof(*a));
    a->socket.fd = assert_accept(serv->fd);
}
static inline void os_tcp_close(struct os_tcp_socket *s) {
    /* Close the TCP socket. */
    shutdown(s->fd, SHUT_WR);
    for (;;) {
        uint8_t buf;
        int rv = read(s->fd, &buf, 1);
        if (rv <= 0) { break; }
        // LOG("flushing %d\n", buf);
    }
    close(s->fd);
}
static intptr_t os_tcp_read(struct os_tcp_socket *s, uint8_t *buf, uintptr_t len) {
    LOG("read...\r");
    // ssize_t rv = assert_read(c->socket, buf, len);
    // FIXME: This can produce a short read.
    ssize_t rv = read(s->fd, buf, len);
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
static intptr_t os_tcp_write(struct os_tcp_socket *s, const uint8_t *buf, uintptr_t len) {
    assert_write(s->fd, buf, len);
    //for(uintptr_t i=0; i<len; i++) LOG("%c", buf[i]);
    return len;
}


#endif



#endif
