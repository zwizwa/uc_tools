#ifndef OS_TCP_BERKELEY_H
#define OS_TCP_BERKELEY_H

#include "tcp_tools.h"
#include "assert_write.h"
#include "assert_read.h"

/* User apio is the blocking_io, so this can just be cast. */
struct os_tcp_socket {
    struct blocking_io io;
    int fd;
    int debug;
};
#define OS_TCP_SOCKET_INIT {.fd = -1}

/* Move socket into different storage, e.g. for moving from transient
   stack storage to a memory pool. */
void os_tcp_socket_move(struct os_tcp_socket *dst, struct os_tcp_socket *src) {
    memcpy(dst,src,sizeof(*dst));
    memset(src,0,sizeof(*src));
}

struct os_tcp_server {
    int fd;
};
// FIXME: this is just generic file read.
static os_error_t os_tcp_read(struct blocking_io *io, uint8_t *buf, uintptr_t len) {
    struct os_tcp_socket *s = (void*)io;
    LOG("read...\r");
    // ssize_t rv = assert_read(c->socket, buf, len);
    // FIXME: This can produce a short read.
    ssize_t rv = read(s->fd, buf, len);
    if (rv > 0) {
        LOG("read ok          \r");
        return OS_OK;
    }
    ASSERT_ERRNO(rv);
    LOG("read EOF, exit thread\n");
    // Any error terminates the thread
    // free(c);  // not sure if this is ok before exit.. just leak it.
    os_thread_exit(NULL);
}
static os_error_t os_tcp_write(struct blocking_io *io, const uint8_t *buf, uintptr_t len) {
    struct os_tcp_socket *s = (void*)io;
    assert_write(s->fd, buf, len);
    //for(uintptr_t i=0; i<len; i++) LOG("%c", buf[i]);
    return OS_OK;
}
static os_error_t os_tcp_server_init(struct os_tcp_server *s, uint16_t port) {
    s->fd = assert_tcp_listen(3456);
    return OS_OK;
}
static inline os_error_t os_tcp_accept(struct os_tcp_server *serv,
                                       struct os_tcp_socket *client) {
    client->io.read  = os_tcp_read;
    client->io.write = os_tcp_write;
    client->fd = assert_accept(serv->fd);
    return OS_OK;
}
static inline void os_tcp_done(struct os_tcp_socket *s) {
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
static inline void os_tcp_disconnect(struct os_tcp_socket *s) {
    /* FIXME: is this correct?  This is used to close from another
       thread, such that a blocking receive will receive an error. */
    shutdown(s->fd, SHUT_WR);
}
static inline const char *os_strerror(os_error_t e) {
    return strerror((intptr_t)e);
}

#endif

