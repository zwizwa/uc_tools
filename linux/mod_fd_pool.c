#ifndef MOD_FD_POOL
#define MOD_FD_POOL

#ifndef SOCKET_TIMEOUT_SEC
#define SOCKET_TIMEOUT_SEC 60
#endif

#include "macros.h"
#include <poll.h>
#include <sys/timerfd.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <netdb.h>
#include <time.h>

#include "pbuf.h"
#include "tcp_tools.h"

/* MISC */
time_t now(void) { return time(NULL); }

/* Growable array of file descriptor annotations. */
struct fd_meta;
struct fd_meta {
    time_t t_last;
    void (*handle)(struct fd_meta *);
    void (*disconnect)(struct fd_meta *);
    int fd;
    struct pbuf pbuf;
    uint8_t pbuf_buf[1024];
};
struct fd_pool {
    int room;
    struct fd_meta *fd_meta;
};
struct fd_pool fd_pool;
#define FD_META_ROOM_INC 1
void fd_pool_init(struct fd_pool *s) {
    s->room = FD_META_ROOM_INC;
    s->fd_meta = malloc(sizeof(struct fd_meta) * s->room);
}
struct fd_meta *fd_meta(struct fd_pool *s, int fd) {
    // LOG("fd_meta fd=%d, room=%d\n", fd, s->room);
    ASSERT(fd < s->room);
    return &s->fd_meta[fd];
}
void fd_meta_alloc(struct fd_pool *s,
                   int fd,
                   void (*handle)(struct fd_meta *),
                   void (*disconnect)(struct fd_meta *)) {
    // LOG("fd_meta_alloc fd=%d, room=%d\n", fd, s->room);

    /* This relies on file descriptors being reasonable low-value
       integers that get re-used after close.  Note that the allocated
       resource does not need to be freed: it will automatically get
       reclaimed when file descriptors get recycled. */
    while (fd >= s->room) {
        s->room += FD_META_ROOM_INC;
        ASSERT(s->fd_meta = realloc(s->fd_meta, sizeof(struct fd_meta) * s->room));
    }
    struct fd_meta *m = fd_meta(s, fd);
    m->t_last = now();
    m->fd = fd;
    m->handle = handle;
    m->disconnect = disconnect;
    PBUF_INIT(m->pbuf);
    ASSERT(m->handle);
};


void fd_meta_handle(struct fd_meta *s,
                    void (*handle_message)(struct fd_meta *),
                    void (*disconnect)(struct fd_meta *)) {

    s->t_last = now();

    /* This assumes the client will send complete messages.  Router
       daemon will freeze until full message is received. FIXME: This
       should be decoupled!  Add buffered receive or switch to
       library. */
    uint32_t room = s->pbuf.size - s->pbuf.count;
    int rv = read(s->fd, s->pbuf.buf + s->pbuf.count, room);
    if (rv <= 0) {
        /* Close connection. */
        LOG("fd_meta_handle disconnect fd=%d rv=%d\n", s->fd, rv);
        disconnect(s);
        return;
    }
    s->pbuf.count += rv;

    for(;;) {
        if (s->pbuf.count < 4) return;
        uint32_t len = read_be(s->pbuf.buf, 4);
        if (s->pbuf.count < 4 + len) return;
        /* Handle one size-prefixed message at start of buffer. */
        handle_message(s);
        /* Clean up buffer. */
        s->pbuf.count -= (4 + len);
        if (s->pbuf.count) {
            memmove(s->pbuf.buf, s->pbuf.buf + 4 + len, s->pbuf.count);
        }
    }
}

int fd_meta_try_connect(
    struct fd_pool *s,
    const char *s_ip,
    uint16_t port,
    void (*handle)(struct fd_meta *),
    void (*disconnect)(struct fd_meta *),
    const char *tag)
{
    int fd;
    /* Start a new server connection. */
    struct sockaddr_in address_in = {
        .sin_port = htons((uint16_t)port),
        .sin_family = AF_INET
    };
    inet_aton(s_ip, &address_in.sin_addr);
    ASSERT_ERRNO(fd = socket(AF_INET, SOCK_STREAM, 0));
    int intarg = -1;
    ASSERT_ERRNO(setsockopt(fd, IPPROTO_TCP, TCP_NODELAY,
                            &intarg, sizeof(intarg)));
    socklen_t addrlen = sizeof(address_in);
    if (-1 == connect(fd, (struct sockaddr *)&address_in, addrlen)) {
        LOG("%s: %s: failed\n", tag, s_ip);
        close(fd);
        return -1;
    }
    else {
        LOG("%s: %s: connected\n", tag, s_ip);
        fd_meta_alloc(s, fd, handle, disconnect);
        // FIXME: Send client notification
        return fd;
    }
}

int fd_meta_unix_dgram_socket(struct fd_pool *p,
                              const char *path,
                              void (*handle)(struct fd_meta*)) {
    int fd;
    struct sockaddr_un un;
    ASSERT(-1 != (fd = socket(PF_UNIX, SOCK_DGRAM,0)));
    un.sun_family = AF_UNIX;
    strcpy(un.sun_path, path);
    socklen_t addrlen = sizeof(un.sun_family) + strlen(un.sun_path) + 1;
    ASSERT(0 == bind(fd, (struct sockaddr *)&un, addrlen));
    fd_meta_alloc(p, fd, handle, NULL);
    return fd;
}

int fd_meta_timer_ms(struct fd_pool *p,
                     void (*handle)(struct fd_meta *),
                     int ms) {
    int clockid = CLOCK_MONOTONIC;
    int create_flags = 0;
    int fd;
    ASSERT_ERRNO(fd = timerfd_create(clockid, create_flags));
    int settime_flags = 0;
    time_t s  =  ms / 1000;
    long   ns = (ms % 1000) * 1000000;
    //LOG("s=%d ns=%d\n", s, ns);
    struct itimerspec itimerspec = {
        .it_interval = { .tv_sec = s, .tv_nsec = ns },
        .it_value    = { .tv_sec = s, .tv_nsec = ns },
    };
    timerfd_settime(fd, settime_flags, &itimerspec, NULL);
    fd_meta_alloc(p, fd, handle, NULL);
    return fd;
}


/* FIXME: Since we are single threaded, we will need to buffer
   outgoing messages as well!  For now this will block. */
static inline int write_nb(int fd, const uint8_t *buf, size_t len, int sleep_ns) {
    size_t written = 0;
    while(written < len) {
        ssize_t rv;
        while ((rv = write(fd, buf + written, len - written)) <= 0) {
            if (EAGAIN == errno) {
                // LOG(warning);
                struct timespec ts = {
                    .tv_sec = 0,
                    .tv_nsec = sleep_ns
                };
                nanosleep(&ts, NULL);
            }
            else {
                LOG("ERROR: write(%d,%p,%d) == %d, errno=%d, strerror=\"%s\"\n",
                    fd, buf, (int)len, (int)rv, (int)errno, strerror(errno));
                return -1;
            }
        }
        written += rv;
    }
    return written;
}

/* Growable array of struct pollfd */
struct poll_state {
    int room;
    int nb_fd;
    struct pollfd *pfd;
    struct fd_pool *p;
} poll_state = {};
void pfd_init(struct poll_state *s, struct fd_pool *p) {
    s->pfd = malloc(sizeof(struct pollfd) * 1);
    s->room = 1;
    s->nb_fd = 0;
    s->p = p;
}
void pfd_clear(struct poll_state *s) {
    s->nb_fd = 0;
}
void pfd_add(struct poll_state *s,
             int fd) {
    struct fd_pool *p = s->p;
    time_t t = now();

    /* All TCP connections use a keepalive mechanism to guard against
       half open connections, where the other end has terminated but
       we don't know about it.  Close the socket if it's been inactive
       for too long. */
    struct fd_meta *m = fd_meta(p, fd);
    if ((t - m->t_last) > SOCKET_TIMEOUT_SEC) {
        if (m->disconnect) {
            LOG("pfd_add timeout disconnect fd=%d\n", fd);
            m->disconnect(m);
            return;
        }
        else {
            /* Absence of disconnect method disables this mechanism,
               e.g. for timer, control socket, which are assumed to be
               always up. */
        }
    }

    // LOG("pfd_add %d\n", fd);
    if (s->room == s->nb_fd) {
        s->room++;
        ASSERT(s->pfd = realloc(s->pfd, sizeof(struct pollfd) * s->room));
    }
    s->pfd[s->nb_fd].fd = fd;
    s->pfd[s->nb_fd].events = POLLIN;
    s->nb_fd++;
};
void pfd_poll(struct poll_state *s,
              struct fd_pool *p) {
    // LOG("poll nb_fd=%d\n", poll_state.nb_fd);
    if (poll(poll_state.pfd, poll_state.nb_fd, -1 /*inf*/) > 0) {
        for (int i=0; i<poll_state.nb_fd; i++) {
            struct pollfd *pfd = &poll_state.pfd[i];
            struct fd_meta *s = fd_meta(p, pfd->fd);
            /* ERRORS */
            if (pfd->revents & (POLLERR | POLLHUP | POLLNVAL)) {
                if (s->disconnect) {
                    s->disconnect(s);
                }
                else {
                    ERROR("error %d\n", pfd->revents);
                }
            }
            /* INPUT */
            else if (pfd->revents & POLLIN) {
                ASSERT(s->handle);
                s->handle(s);
            }
        }
    }
    else {
        ERROR("poll error\n");
    }
}




#endif
