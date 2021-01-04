/*
Copyright 2019-2020 (c) Tom Schouten

Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions
are met:

1. Redistributions of source code must retain the above copyright
   notice, this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright
   notice, this list of conditions and the following disclaimer in the
   documentation and/or other materials provided with the
   distribution.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
"AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
(INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/* Bridge two packet-based interfaces.
   E.g. works like socat, but for UDP, TAP, SLIP stream, {packet,4} stream, ...
*/

/*
   Note that this is a testing tool that should probably not be
   operated on a non-trusted network.  Don't shoot yourself in the
   foot, because security measures are minimal.

   For ease of configuration, UDP packets are returned to the first
   peer that sends a message to a listening socket and all other peers
   are ignored afterwards.  E.g. if you bridge TAP and UDP and know
   the UDP address, you can essentially gain unrestricted raw Ethernet
   access to whatever the tap interface is bridged to.
*/

/* FIXME: Document this properly. Other parts of uc_tools have this
   configured incorrectly. */
#define _POSIX_C_SOURCE 199309L
#include <sys/timerfd.h>

#include "packet_bridge.h"
#include "tcp_tools.h"

#include "macros.h"

#include <stdio.h>
#include <string.h>
#include <stdint.h>

#include <fcntl.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <sys/ioctl.h>
#include <sys/socket.h>
#include <sys/un.h>

#include <linux/if.h>
#include <linux/if_tun.h>

#include <unistd.h>

#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <poll.h>

#include <netdb.h>

#include <signal.h>

//#include "/usr/include/asm-generic/termbits.h"
//#include "/usr/include/asm-generic/ioctls.h"

#include <asm-generic/termbits.h>
#include <asm-generic/ioctls.h>

#include "assert_write.h"


/* A note on USB bulk transfers:
   The async libusb interface does not fit packet_bridge very well.

   For lack of better wording, the "short packet termination" protocol
   associated with USB bulk transfers has been implemented in a
   separate process (erl_tools/c_src/axo_connect.c) that can be used
   in conjuction with packet_bridge EXEC port. */



/***** 1. PACKET INTERFACES */


// https://stackoverflow.com/questions/1003684/how-to-interface-with-the-linux-tun-driver
// https://www.kernel.org/doc/Documentation/networking/tuntap.txt
// https://wiki.wireshark.org/Development/LibpcapFileFormat


static void __attribute__((unused)) log_addr(struct sockaddr_in *sa) {
    uint8_t *a = (void*)&sa->sin_addr;
    LOG("%d.", a[0]);
    LOG("%d.", a[1]);
    LOG("%d.", a[2]);
    LOG("%d:", a[3]);
    LOG("%d\n", ntohs(sa->sin_port));
}
static inline void log_hex(uint8_t *buf, ssize_t n) {
    for(ssize_t i=0; i<n; i++) {
        LOG(" %02x", buf[i]);
        if (i%16 == 15) LOG("\n");
    }
    LOG("\n");
}
// FIXME: This is ad-hoc annotation for a bug hunt.
static inline void log_str(const uint8_t *m, ssize_t len) {
    LOG("[");
    for (ssize_t i=0; i<len; i++) {
        uint8_t c = m[i];
        if ((c >= 32) && (c <= 126)) {
            LOG("%c", c);
        }
        else {
            LOG("\\x%02x", c);
        }
    }
    LOG("]\n");
}




// FIXME: Only IPv4 For now
static int __attribute__((unused)) same_addr(struct sockaddr_in *sa1, struct sockaddr_in *sa2) {
    uint8_t *a1 = (void*)&sa1->sin_addr;
    uint8_t *a2 = (void*)&sa2->sin_addr;
    int same = (sa1->sin_port == sa2->sin_port)
        && (a1[0] == a2[0])
        && (a1[1] == a2[1])
        && (a1[2] == a2[2])
        && (a1[3] == a2[3]);
    return same;
}



/***** 1.1. TAP PORT */

static ssize_t tap_read(struct port *p, uint8_t *buf, ssize_t len) {
    ssize_t rlen;
    ASSERT_ERRNO(rlen = read(p->fd, buf, len));
    return rlen;
}
static ssize_t tap_write(struct port *p, const uint8_t *buf, ssize_t len) {
    // EIO is normal until iface is set up
    return write(p->fd, buf, len);
}

struct port *port_open_tundev(const char *dev, int flags) {
    int fd;
    ASSERT_ERRNO(fd = open("/dev/net/tun", O_RDWR));
    struct ifreq ifr = { .ifr_flags = flags };
    strncpy(ifr.ifr_name, dev, IFNAMSIZ);
    ASSERT_ERRNO(ioctl(fd, TUNSETIFF, (void *) &ifr));
    LOG("tap: %s\n", dev);
    struct port *port;
    ASSERT(port = malloc(sizeof(*port)));
    port->fd = fd;
    port->fd_out = fd;
    port->read = tap_read;
    port->write = tap_write;
    port->pop = 0;
    return port;
}


/***** 1.2. UDP PORT */

static ssize_t udp_read(struct udp_port *p, uint8_t *buf, ssize_t len) {
    //LOG("udp_read\n");
    ssize_t rlen = 0;
    int flags = 0;
    struct sockaddr_in peer = {};
    socklen_t addrlen = sizeof(peer);
    ASSERT_ERRNO(
        rlen = recvfrom(p->p.fd, buf, len, flags,
                        (struct sockaddr*)&peer, &addrlen));
    ASSERT(addrlen == sizeof(peer));

    /* Associate to the last peer that sends to us.  This is to make
       setup simpler. */
    /*
    if (!same_addr(&p->peer, &peer)) {
        LOG("peer:"); log_addr(&peer);
    }
    */
    memcpy(&p->peer, &peer, sizeof(peer));

    //LOG("udp_read %d\n", rlen);
    return rlen;

}
static ssize_t udp_write(struct udp_port *p, uint8_t *buf, ssize_t len) {
    if (p->peer.sin_port == 0) {
        /* Drop while not assicated */
        return 0;
    }
    ssize_t wlen;
    int flags = 0;
    // LOG("udp_write: %d\n", len);
    ASSERT_ERRNO(
        wlen = sendto(p->p.fd, buf, len, flags,
                      (struct sockaddr*)&p->peer,
                      sizeof(p->peer)));
    return wlen;
}

struct port *port_open_udp(uint16_t port) {
    int fd;
    ASSERT_ERRNO(fd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP));
    if(port) {
        struct sockaddr_in address = {
            .sin_port = htons(port),
            .sin_family = AF_INET
        };
        ASSERT_ERRNO(setsockopt(fd, SOL_SOCKET, SO_REUSEADDR, &(int){ 1 }, sizeof(int)));
        socklen_t addrlen = sizeof(address);
        ASSERT_ERRNO(bind(fd, (struct sockaddr *)&address, addrlen));
        LOG("udp: port %d\n", port);
    }
    else {
        LOG("udp: not bound\n");
    }
    struct udp_port *p;
    ASSERT(p = malloc(sizeof(*p)));
    memset(p,0,sizeof(*p));
    p->p.fd = fd;
    p->p.fd_out = fd;
    p->p.read  = (port_read_fn)udp_read;
    p->p.write = (port_write_fn)udp_write;
    p->p.pop = 0;

    return &p->p;
}

/***** 1.3 TIMERFD */

/* See man 2 timerfd_create.  The packet consists of a uint64_t.  One
   is returned per expiration. */
struct timerfd_port {
    struct port p;
};
static ssize_t timerfd_read(struct timerfd_port *p, uint8_t *buf, ssize_t len) {
    return read(p->p.fd, buf, sizeof(uint64_t));
}
static ssize_t timerfd_write(struct timerfd_port *p, uint8_t *buf, ssize_t len) {
    LOG("WARNING: timerfd is read-only\n");
    return len;
}
struct port *port_open_timerfd_stream(long ms) {
    struct timerfd_port *p;
    ASSERT(p = malloc(sizeof(*p)));
    memset(p,0,sizeof(*p));
    int clockid = CLOCK_MONOTONIC;
    int create_flags = 0;
    int fd;
    ASSERT_ERRNO(fd = timerfd_create(clockid, create_flags));
    p->p.fd = fd;
    p->p.fd_out = fd; // not used
    p->p.read  = (port_read_fn)timerfd_read;
    p->p.write = (port_write_fn)timerfd_write;
    int settime_flags = 0;
    time_t s  =  ms / 1000;
    long   ns = (ms % 1000) * 1000000;
    //LOG("s=%d ns=%d\n", s, ns);
    struct itimerspec itimerspec = {
        .it_interval = { .tv_sec = s, .tv_nsec = ns },
        .it_value    = { .tv_sec = s, .tv_nsec = ns },
    };
    timerfd_settime(fd, settime_flags, &itimerspec, NULL);
    return &p->p;
}



/***** 2. STREAM INTERFACES */

/* Stream interfaces need two layers: a low level FD layer, and a
   framing layer: packetn or slip. */


/***** 2.1  LOW LEVEL FD */


/* Low-level open routines that return one or two file descriptors to
   support multiple framing protocols. */
static void fd_open_tty(int *pfd, const char *dev) {
    int fd;

    // FIXME: Explain why this should be non-blocking?
    ASSERT_ERRNO(fd = open(dev, O_RDWR | O_NONBLOCK));
    // ASSERT_ERRNO(fd = open(dev, O_RDWR));

    struct termios2 tio;
    ASSERT(0 == ioctl(fd, TCGETS2, &tio));

    // http://www.cs.uleth.ca/~holzmann/C/system/ttyraw.c
    tio.c_iflag &= ~(BRKINT | ICRNL | INPCK | ISTRIP | IXON);
    tio.c_oflag &= ~(OPOST);
    tio.c_cflag |= (CS8);
    tio.c_lflag &= ~(ECHO | ICANON | IEXTEN | ISIG);
    tio.c_cc[VMIN] = 1;
    tio.c_cc[VTIME] = 0;

    ASSERT(0 == ioctl(fd, TCSETS2, &tio));
    *pfd = fd;
}
static void handle_signal(int sig) {
    LOG("SIGNAL %d\n", sig);
    exit(1);
}
static void fd_open_command(int *in_fd, int *out_fd, const char **argv) {
    // This is confusing, so using long names.
    int ignore;
    const int read_end = 0;
    const int write_end = 1;
    int parent_to_child[2];
    int child_to_parent[2];
    ASSERT_ERRNO(pipe(parent_to_child));
    ASSERT_ERRNO(pipe(child_to_parent));

    int pid = fork();

    /* CHILD */
    if (!pid){
        /* replace stdio with pipes and leave stderr as-is. */
        close(0); ignore = dup(parent_to_child[read_end]);
        close(1); ignore = dup(child_to_parent[write_end]);
        (void)ignore;

        /* No longer needed */
        close(parent_to_child[read_end]);
        close(parent_to_child[write_end]);
        close(child_to_parent[read_end]);
        close(child_to_parent[write_end]);

        /* execvp requires NULL-terminated array
           FIXME: only supporting single command, no args */
        ASSERT_ERRNO(execvp(argv[0], (char **)&argv[0]));
        /* not reached (exec success or assert error exit) */
    }

    signal(SIGCHLD, handle_signal);

    /* PARENT */
    *in_fd  = child_to_parent[read_end];  close(child_to_parent[write_end]);
    *out_fd = parent_to_child[write_end]; close(parent_to_child[read_end]);
}

#if 0
static void fd_open_sock_dgram(int *pfd, const char *path) {
    int fd;
    struct sockaddr_un un;

    ASSERT_ERRNO(fd = socket(PF_UNIX, SOCK_STREAM,0));
    un.sun_family = AF_UNIX;
    strcpy(un.sun_path, path); // FIXME: strncpyUNIX_PATH_MAX
    socklen_t addrlen = sizeof(un.sun_family) + strlen(un.sun_path) + 1;

    // server socket
    if (1) {
        ASSERT(-1 == bind(fd, (struct sockaddr *)&un, addrlen));
    }
    // client socket
    else {
        ASSERT(-1 == connect(fd, (struct sockaddr *)&un, addrlen));
    }

    *pfd = fd;
}
#endif

/***** 2.2. PACKETN FRAMING */


/* For byte streams, some kind of framing is necessary, so use Erlang
 * {packet,N} where every packet is prefixed with a big endian short
 * unsigned int with packet length. */

/* The read method can be shared, parameterized by a protocol-specific
 * "pop" method that attempts to read a packet from the buffer. */

struct buf_port {
    struct port p;
    uint32_t count;
    uint8_t buf[2*PACKET_BRIDGE_MAX_PACKET_SIZE];
};
static ssize_t pop_read(port_pop_fn pop,
                        struct buf_port *p, uint8_t *buf, ssize_t len) {
    ssize_t size;
    /* If we still have a packet, return that first. */
    if ((size = pop(p, buf, len))) return size;

    /* We get only one read() call, so make it count. */
    uint32_t room = sizeof(p->buf) - p->count;
    //LOG("packetn_read %d\n", p->count);
    ssize_t rv = read(p->p.fd, &p->buf[p->count], room);
    if (rv > 0) {
        //LOG("pop_read:\n");
        //log_hex(&p->buf[p->count], rv);
        // log_str(&p->buf[p->count], rv);
    }
    //LOG("packetn_read done %d\n", rv);
    if (rv == -1) {
        switch (errno) {
        case EAGAIN:
            return 0;
        default:
            ASSERT_ERRNO(-1);
        }
    }
    if (rv == 0) {
        ERROR("eof\n");
    }
    ASSERT(rv > 0);
    p->count += rv;
    return pop(p, buf, len);
}



struct packetn_port {
    struct buf_port p;
    uint32_t len_bytes;
};

uint32_t packetn_packet_size(struct packetn_port *p) {
    ASSERT(p->len_bytes <= 4);
    ASSERT(p->p.count >= p->len_bytes);
    uint32_t size = 0;
    for (uint32_t i=0; i<p->len_bytes; i++) {
        size = (size << 8) + p->p.buf[i];
    }
    //LOG("size %d\n", size);
    return size;
}
uint32_t packetn_packet_write_size(struct packetn_port *p, uint32_t size, uint8_t *buf) {
    ASSERT(p->len_bytes <= 4);
    for (uint32_t i=0; i<p->len_bytes; i++) {
        buf[p->len_bytes-1-i] = size & 0xFF;
        size = size >> 8;
    }
    return size;
}

static ssize_t packetn_pop(struct packetn_port *p, uint8_t *buf, ssize_t len) {
    uint32_t size;

  again:

    /* Make sure there are enough bytes to get the size field. */
    if (p->p.count < p->len_bytes) return 0;
    size = packetn_packet_size(p);

    /* Packets are assumed to fit in the buffer.  An error here is
     * likely a bug or a protocol {packet,N} framing error. */
    if (sizeof(p->p.buf) < p->len_bytes + size) {
        ERROR("buffer overflow for stream packet size=%d(0x%08x)\n", size, size);
    }

    /* Ensure packet is complete and fits in output buffer before
     * copying.  Skip the size prefix, which is used only for stream
     * transport framing. */
    if (p->p.count < p->len_bytes + size) return 0;
    ASSERT(size <= len);
    memcpy(buf, &p->p.buf[p->len_bytes], size);
    //LOG("copied %d:\n", size);
    //log_hex(buf, size);


    /* If there is anything residue, move it to the front. */
    if (p->p.count == p->len_bytes+size) {
        p->p.count = 0;
    }
    else {
        ssize_t head_count = p->len_bytes + size;
        ssize_t tail_count = p->p.count - head_count;
        memmove(&p->p.buf[0], &p->p.buf[head_count], tail_count);
        //LOG("moved %d %d:\n", p->count, tail_count);
        //log_hex(&p->buf[0],tail_count);
        p->p.count = tail_count;
    }
    //LOG("pop: %d %d\n", size, p->count);

    /* Drop empty packets. */
    if (0 == size) goto again;

    return size;
}

static ssize_t packetn_read(struct packetn_port *p, uint8_t *buf, ssize_t len) {
    return pop_read((port_pop_fn)packetn_pop, &p->p, buf, len);
}

static ssize_t packetn_write(struct packetn_port *p, uint8_t *buf, ssize_t len) {
    int fd = p->p.p.fd_out;
    //LOG("packetn_write %d\n", len);
    // Buffer and copy the data so we can use a single write.
    uint32_t packet_len = p->len_bytes + len;
    uint8_t packet[packet_len];
    packetn_packet_write_size(p, len, &packet[0]);
    memcpy(&packet[p->len_bytes], buf, len);
    assert_write(fd, packet, packet_len);
    //LOG("packetn_write %d (done)\n", len);
    return len + p->len_bytes;
}
struct port *port_open_packetn_stream(uint32_t len_bytes, int fd, int fd_out) {
    struct packetn_port *p;
    ASSERT(p = malloc(sizeof(*p)));
    memset(p,0,sizeof(*p));
    p->p.p.fd = fd;
    p->p.p.fd_out = fd_out;
    p->p.p.read  = (port_read_fn)packetn_read;
    p->p.p.write = (port_write_fn)packetn_write;
    p->p.p.pop   = (port_pop_fn)packetn_pop;
    p->len_bytes = len_bytes;
    return &p->p.p;
}



struct port *port_open_packetn_tty(uint32_t len_bytes, const char *dev) {
    int fd;
    fd_open_tty(&fd, dev);
    return port_open_packetn_stream(len_bytes, fd, fd);
}


struct port *port_open_packetn_command(uint32_t len_bytes, const char **argv) {
    int in_fd, out_fd;
    fd_open_command(&in_fd, &out_fd, argv);
    return port_open_packetn_stream(len_bytes, in_fd, out_fd);
}

struct port *port_open_packetn_tcp_connect(
    uint32_t len_bytes, const char *host, uint16_t tcp_port) {
    int fd = assert_tcp_connect(host, tcp_port);
    return port_open_packetn_stream(len_bytes, fd, fd);
}


/***** 2.3. SLIP FRAMING */


/* See https://en.wikipedia.org/wiki/Serial_Line_Internet_Protocol */
#define SLIP_END     0xC0
#define SLIP_ESC     0xDB
#define SLIP_ESC_END 0xDC
#define SLIP_ESC_ESC 0xDD

/* For byte streams, some kind of framing is necessary, so use Erlang
 * {packet,N} where every packet is prefixed with a big endian short
 * unsigned int with packet length. */

struct slip_port {
    struct buf_port p;
};

/* Try to pop a frame.  If it's not complete, return 0.  p->buf
 * contains slip-encoded data.  It is allowed to use the output buffer
 * to perform partial decoding. */

/* Note: empty packets occur frequently in slip streams, but
   packet_bridge does not support those, as a zero return value in
   this function indicates no more data.  FIXME: That should probably
   change, but for now keep the refactoring effort minimal and drop
   empty frames. */
static ssize_t slip_pop(struct slip_port *p, uint8_t *buf, ssize_t len) {

    // 1. Go over data and stop at packet boundary, writing partial
    // data to output buffer.  Abort with 0 size when packet is
    // incomplete.
    ssize_t in, out;

  again:
    in = 0;
    out = 0;

    for(;;) {
        ASSERT(out < len);
        if (in >= p->p.count) {
            /* Attempt to read past buffer. */
            return 0;
        }
        uint8_t c = p->p.buf[in++];
        if (SLIP_END == c) {
            break;
        }
        else if (SLIP_ESC == c) {
            if (in >= p->p.count) {
                /* Attempt to read past buffer. */
                return 0;
            }
            uint8_t c = p->p.buf[in++];
            if (SLIP_ESC_ESC == c) {
                buf[out++] = SLIP_ESC;
            }
            else if (SLIP_ESC_END == c) {
                buf[out++] = SLIP_END;
            }
            else {
                ERROR("bad slip escape %d\n", (int)c);
            }
        }
        else {
            buf[out++] = c;
        }
    }

    //LOG("slip_pop: "); log_hex(&p->p.buf[0], in);


    // 2. Shift the data buffer.
    /* We can only get here after reading at least one character.
       This ensures we will not get stuck in the loop caused by "goto
       again". */
    ASSERT(in > 0);
    memmove(&p->p.buf[0], &p->p.buf[in], p->p.count-in);
    p->p.count -= in;

    // 3. Drop empty frames.  See comment above
    if (0 == out) goto again;

    /* if (p->p.count > 0) { */
    /*     LOG("left after slip_pop: "); */
    /*     log_hex(&p->p.buf[0], p->p.count); */
    /* } */

    return out;
}
static ssize_t slip_read(struct packetn_port *p, uint8_t *buf, ssize_t len) {
    return pop_read((port_pop_fn)slip_pop, &p->p, buf, len);
}

static ssize_t slip_write(struct slip_port *p, uint8_t *buf, ssize_t len) {
    /* Use a temporary buffer to avoid multiple write() calls.  Worst
     * case size is x2, when each character is escaped, plus 2 for
     * double-ended delimiters. */
    uint8_t tmp[len*2 + 2];

    ssize_t out = 0;

    /* Convention: write packet boundary at the beginning and the
     * start.  Receiver needs to throw away empty (or otherwise
     * invalid) packets. */
    tmp[out++] = SLIP_END;
    for(ssize_t in=0; in<len; in++) {
        int c_in = buf[in];
        if (SLIP_END == c_in) {
            tmp[out++] = SLIP_ESC;
            tmp[out++] = SLIP_ESC_END;
        }
        else if (SLIP_ESC == c_in) {
            tmp[out++] = SLIP_ESC;
            tmp[out++] = SLIP_ESC_ESC;
        }
        else {
            tmp[out++] = c_in;
        }
    }
    tmp[out++] = SLIP_END;

    //LOG("slip_write: "); log_hex(tmp, out);

    assert_write(p->p.p.fd_out, tmp, out);
    return out;
}


/***** 2.4. SYSTEM FRAMING */

/* rely on read/write system call being packet-delimited. */

static ssize_t sys_read(struct port *p, uint8_t *buf, ssize_t len) {
    ssize_t rlen;
    ASSERT_ERRNO(rlen = read(p->fd, buf, len));
    if (rlen == 0) {
        ERROR("eof\n");
    }
    return rlen;
}
static ssize_t sys_write(struct port *p, const uint8_t *buf, ssize_t len) {
    // EIO is normal until iface is set up
    return write(p->fd, buf, len);
}

struct port *port_open_sys(int fd, int fd_out) {
    struct port *port;
    ASSERT(port = malloc(sizeof(*port)));
    port->fd = fd;
    port->fd_out = fd_out;
    port->read = sys_read;
    port->write = sys_write;
    port->pop = 0;
    return port;
}







struct port *port_open_slip_stream(int fd, int fd_out) {
    struct slip_port *p;
    ASSERT(p = malloc(sizeof(*p)));
    memset(p,0,sizeof(*p));
    p->p.p.fd = fd;
    p->p.p.fd_out = fd_out;
    p->p.p.read  = (port_read_fn)slip_read;
    p->p.p.write = (port_write_fn)slip_write;
    p->p.p.pop   = (port_pop_fn)slip_pop;
    return &p->p.p;
}
struct port *port_open_slip_tty(const char *dev) {
    int fd;
    fd_open_tty(&fd, dev);
    return port_open_slip_stream(fd, fd);
}
struct port *port_open_slip_command(const char **argv) {
    ASSERT(0); // not yet implemented
}
struct port *port_open_slip_tcp_connect(const char *host, uint16_t tcp_port) {
    ASSERT(0); // not yet implemented
}


/***** 2.5. HEX+LINE FRAMING */

struct hex_port {
    struct buf_port p;
    FILE *f_out;
};


static int hexdigit(int c) {
    if ((c >= '0') && (c <= '9')) return c - '0';
    if ((c >= 'A') && (c <= 'F')) return c - 'A' + 10;
    if ((c >= 'a') && (c <= 'f')) return c - 'a' + 10;
    return -1;
}


static ssize_t hex_pop(struct hex_port *p, uint8_t *buf, ssize_t len) {

    // 1. Go over data and stop at packet boundary, writing partial
    // data to output buffer.  Abort with 0 size when packet is
    // incomplete.
    ssize_t in = 0, out = 0;
    for(;;) {
        ASSERT(out < len);

        if (in >= p->p.count) return 0;
        uint8_t c1 = p->p.buf[in++];

        /* Check any control characters. */
        if (c1 == '\n') {
            /* Newline terminates. */
            break;
        };
        if (c1 == ' ') {
            /* Spaces are allowed inbetween hex bytes. */
            continue;
        }

        /* The only legal case left is two valid hex digits. */
        if (in >= p->p.count) return 0;
        uint8_t c2 = p->p.buf[in++];

        int d1, d2;
        ASSERT(-1 != (d1 = hexdigit(c1)));
        ASSERT(-1 != (d2 = hexdigit(c2)));

        buf[out++] = (d1 << 4) + d2;
    }

    // 2. Shift the data buffer
    memmove(&p->p.buf[0], &p->p.buf[in], p->p.count-in);
    p->p.count -= in;

    return out;
}


static ssize_t hex_read(struct hex_port *p, uint8_t *buf, ssize_t len) {
    return pop_read((port_pop_fn)hex_pop, &p->p, buf, len);
}
static ssize_t hex_write(struct hex_port *p, uint8_t *buf, ssize_t len) {
    ssize_t out = 0;
    for (ssize_t i=0; i<len; i++) { out += fprintf(p->f_out, " %02x", buf[i]); }
    out += fprintf(p->f_out, "\n");
    fflush(p->f_out);
    return out;
}
struct port *port_open_hex_stream(int fd, int fd_out) {
    struct hex_port *p;
    ASSERT(p = malloc(sizeof(*p)));
    memset(p,0,sizeof(*p));
    p->p.p.fd = fd;
    p->p.p.fd_out = fd_out;
    p->p.p.read  = (port_read_fn)hex_read;
    p->p.p.write = (port_write_fn)hex_write;
    p->p.p.pop   = (port_pop_fn)hex_pop;
    p->f_out = fdopen(fd_out, "w");
    return &p->p.p;
}


/***** 2.6. LINE FRAMING */

/* Convert lines to packets.  Note that this does not support any
   escape characters, so cannot transport 8-bit clean payloads. */

struct line_port {
    struct buf_port p;
    FILE *f_out;
};

static ssize_t line_pop(struct line_port *p, uint8_t *buf, ssize_t len) {

    // 1. Go over data and stop at packet boundary, writing partial
    // data to output buffer.  Abort with 0 size when packet is
    // incomplete.
    ssize_t in = 0, out = 0;
    for(;;) {
        ASSERT(out < len);

        if (in >= p->p.count) return 0;
        uint8_t c1 = p->p.buf[in++];

        /* Copy verbatim.  End-of-line character is part of the data payload. */
        buf[out++] = c1;

        /* Control character determines stop condition. */
        if (c1 == '\n') {
            /* Newline terminates. */
            // LOG("line: %d chars\n", in);
            break;
        };
    }

    // 2. Shift the data buffer
    memmove(&p->p.buf[0], &p->p.buf[in], p->p.count-in);
    p->p.count -= in;

    return out;
}


static ssize_t line_read(struct line_port *p, uint8_t *buf, ssize_t len) {
    return pop_read((port_pop_fn)line_pop, &p->p, buf, len);
}
static ssize_t line_write(struct line_port *p, uint8_t *buf, ssize_t len) {
    assert_write(p->p.p.fd_out, buf, len);
    return len;
}
struct port *port_open_line_stream(int fd, int fd_out) {
    struct line_port *p;
    ASSERT(p = malloc(sizeof(*p)));
    memset(p,0,sizeof(*p));
    p->p.p.fd = fd;
    p->p.p.fd_out = fd_out;
    p->p.p.read  = (port_read_fn)line_read;
    p->p.p.write = (port_write_fn)line_write;
    p->p.p.pop   = (port_pop_fn)line_pop;
    p->f_out = fdopen(fd_out, "w");
    return &p->p.p;
}


/***** 3. PACKET HANDLER */

/* Default behavior for the stand-alone program is to just forward a
 * packet to the other port.  Any other processing behavior is left to
 * application code that uses packet_bridge as a library.. */

void packet_forward(struct packet_handle_ctx *x, int from, const uint8_t *buf, ssize_t len) {
    int to = (from == 0) ? 1 : 0;
    x->port[to]->write(x->port[to], buf, len);
}






/***** 4. FORWARDER */

void packet_loop(packet_handle_fn handle,
                 struct packet_handle_ctx *ctx) {
    //const char progress[] = "-\\|/";
    uint32_t count = 0;

    struct pollfd pfd[ctx->nb_ports];
    memset(pfd, 0, sizeof(pfd));

    for(;;) {
        uint8_t buf[PACKET_BRIDGE_MAX_PACKET_SIZE]; // FIXME: Make this configurable
        int rv;
        // LOG("timeout %d\n", ctx->timeout);
        for (int i=0; i<ctx->nb_ports; i++) {
            pfd[i].fd = ctx->port[i]->fd;
            pfd[i].events = ctx->port[i]->events ?
                            ctx->port[i]->events : POLLIN;
            pfd[i].revents = 0;
        }

        ASSERT_ERRNO(rv = poll(&pfd[0], ctx->nb_ports, ctx->timeout));
        ASSERT(rv >= 0);
        // LOG("poll rv: %d\n", rv);
        if (rv == 0) {
            /* Handler is notified of timeouts. */
            const int timeout_port = -1;
            handle(ctx, timeout_port, NULL, 0);
        }
        else {
            for (int i=0; i<ctx->nb_ports; i++) {
                if( pfd[i].revents & ctx->port[i]->events ||
                    pfd[i].revents & POLLIN) {
                    struct port *in  = ctx->port[i];

                    int rlen;

                    /* The read calls the underlying OS read method
                       only once, so we are guaranteed to not block. */
                    rlen = in->read(in, buf, sizeof(buf));
                    if (rlen) {
                        handle(ctx, i, buf, rlen);
                        count++;
                    }
                    else {
                        /* Port handler read data but dropped it. */
                    }

                    /* For streaming ports, it is possible that the OS
                     * read method returned multiple packets, so we pop
                     * them one by one. */
                    if (in->pop) {
                        struct buf_port *b_in = (struct buf_port*)in;
                        while((rlen = in->pop(b_in, buf, sizeof(buf)))) {
                            handle(ctx, i, buf, rlen);
                            count++;
                        }
                        if(b_in->count) {
                            //LOG("linger:");
                            //log_str(b_in->buf, b_in->count);
                            //log_hex(b_in->buf, b_in->count);
                        }
                    }
                }
                else if (pfd[i].revents) {
                    /* Anything else is an error, e.g. POLLHUP */
                    ERROR("port %d: revents=0x%x\n", i, pfd[i].revents);
                }
            }
        }
    }
}

/***** 5. RPC */

/* Sometimes it is useful to perform blocking RPC on a port in
   response to some message.  I.e. perform a transaction on one port,
   while holding off on handling messages from the other.

   The function below implements the receiving end of such RPCs.  It
   will receive one message, or time out.

   Typical scenario:
   - ASSERT(sizeof(req_buf) == p->write(p, req_buf, req_size));
   - call packet_next() until timeout or until a reply arrives

   Note that in many setups it might be necessary to keep forwarding
   packets that are not replies.  This is a "mixed" fowarder/rpc
   setup.

*/



/* Get the next packet from a port.  Returns 0 on timeout. */
ssize_t packet_next(struct port *p, int timeout,
                    uint8_t *buf, ssize_t buf_size) {

    ssize_t rlen;
    for(;;) {

        /* Check buffer first. */
        if (p->pop) {
            rlen = p->pop((struct buf_port *)p, buf, buf_size);
            if (rlen) goto done;
        }

        /* Wait for event with timeout. */
        struct pollfd pfd = {};
        pfd.fd = p->fd;
        pfd.events = POLLERR | POLLIN;
        int rv;
        ASSERT_ERRNO(rv = poll(&pfd, 1, timeout));

        /* No event.  Signal caller with empty packet. */
        if (rv == 0) {
            if (0) {
                // HACK: bug hunting... try a single read anyway.
                rlen = p->read(p, buf, buf_size);
                if (rlen) {
                    LOG("WARNING: read returned data after poll failed\n");
                }
                goto done;
            }

            rlen = 0;
            goto done;
        }

        ASSERT(pfd.revents & POLLIN);

        /* The read calls the underlying OS read method only once, so we
         * are guaranteed to not block. */
        rlen = p->read(p, buf, buf_size);
        if (rlen) goto done;

        /* There was an OS read, but no packet was produced.  Retry
           read.  Eventually we exit this loop due to data or
           timeout. */

        /* FIXME: We "reset" the timeout on each poll.  A proper
           implementation would check the actual elapsed time and
           adjust the remaining time accordingly. */
    }
  done:
    if(0) {
        if (!rlen) {
            /* This only happens after timeout. */
            LOG("packet_next: timeout.  buffer: ");
            struct buf_port *bp = (void*)p;
            log_hex(bp->buf, bp->count);
        }
        else {
            LOG("packet_next:\n"); log_hex(buf, rlen);
        }
    }
    return rlen;
}


/***** 6. CONSTRUCTORS */

static void strtok_argv(char *tok, const char *delim,
                        char const **argv, int room) {
    for (int i=0;;i++) {
        ASSERT(i<room);
        if (!(argv[i] = strtok(NULL, delim))) break;
        //LOG("strtok_argv %d %s\n", i, argv[i]);
    }
}


struct port *port_open(const char *spec_ro) {
    char spec[strlen(spec_ro)+1];
    strcpy(spec, spec_ro);

    const char delim[] = ":";
    char *tok;
    ASSERT(tok = strtok(spec, delim));

    // 1. PACKET INTERFACES

    // TAP:<tundev>
    if (!strcmp(tok, "TAP")) {
        ASSERT(tok = strtok(NULL, delim));
        const char *tundev = tok;
        ASSERT(NULL == (tok = strtok(NULL, delim)));
        //LOG("TAP:%s\n", tundev);
        return port_open_tundev(tundev, IFF_TAP | IFF_NO_PI);
    }

    if (!strcmp(tok, "TUN")) {
        ASSERT(tok = strtok(NULL, delim));
        const char *tundev = tok;
        ASSERT(NULL == (tok = strtok(NULL, delim)));
        //LOG("TAP:%s\n", tundev);
        return port_open_tundev(tundev, IFF_TUN | IFF_NO_PI);
    }

    // UDP-LISTEN:<port>
    if (!strcmp(tok, "UDP-LISTEN")) {
        ASSERT(tok = strtok(NULL, delim));
        uint16_t port = atoi(tok);
        ASSERT(NULL == (tok = strtok(NULL, delim)));
        //LOG("UDP-LISTEN:%d\n", port);
        return port_open_udp(port);
    }

    // UDP:<host>:<port>
    if (!strcmp(tok, "UDP")) {
        ASSERT(tok = strtok(NULL, delim));
        const char *host = tok;
        ASSERT(tok = strtok(NULL, delim));
        uint16_t port = atoi(tok);
        ASSERT(NULL == (tok = strtok(NULL, delim)));
        //LOG("UDP:%s:%d\n", host, port);

        struct port *p = port_open_udp(0); // don't spec port here
        struct udp_port *up = (void*)p;

        assert_gethostbyname(&up->peer, host);
        up->peer.sin_port = htons(port);
        up->peer.sin_family = AF_INET;

        return p;
    }

    // 2. FRAME INTERFACES
    // First argument is always the framing type.
    // Currently only two kinds are supported:
    // - slip
    // - little-endian size prefix

    // TTY:<framing>:<dev>
    if (!strcmp(tok, "TTY")) {
        ASSERT(tok = strtok(NULL, delim));
        if (!strcmp("slip", tok)) {
            ASSERT(tok = strtok(NULL, delim));
            const char *dev = tok;
            ASSERT(NULL == (tok = strtok(NULL, delim)));
            //LOG("port_open_slip_tty(%s)\n", dev);
            return port_open_slip_tty(dev);
        }
        else {
            uint16_t len_bytes = atoi(tok);
            ASSERT(tok = strtok(NULL, delim));
            const char *dev = tok;
            ASSERT(NULL == (tok = strtok(NULL, delim)));
            return port_open_packetn_tty(len_bytes, dev);
        }
    }




    // EXEC:<framing>:<command>
    if (!strcmp(tok, "EXEC")) {
        char const* argv[PACKET_BRIDGE_MAX_EXEC_ARGC] = {};
        ASSERT(tok = strtok(NULL, delim));
        if (!strcmp("slip", tok)) {
            strtok_argv(tok, delim, argv, ARRAY_SIZE(argv));
            ASSERT(argv[0]);
            return port_open_slip_command(argv);
        }
        else {
            uint16_t len_bytes = atoi(tok);
            strtok_argv(tok, delim, argv, ARRAY_SIZE(argv));
            ASSERT(argv[0]);
            return port_open_packetn_command(len_bytes, argv);
        }
    }

    // TCP:<framing>:<host>:<port>
    if (!strcmp(tok, "TCP")) {
        ASSERT(tok = strtok(NULL, delim));
        const char *framing = tok;
        ASSERT(tok = strtok(NULL, delim));
        const char *host = tok;
        ASSERT(tok = strtok(NULL, delim));
        uint16_t tcp_port = atoi(tok);
        ASSERT(NULL == (tok = strtok(NULL, delim)));
        //LOG("TCP:%s:%d\n", host, port);

        if (!strcmp("slip", framing)) {
            return port_open_slip_tcp_connect(host, tcp_port);
        }
        else {
            uint16_t len_bytes = atoi(framing);
            return port_open_packetn_tcp_connect(len_bytes, host, tcp_port);
        }
    }

    if (!strcmp(tok, "TIMERFD")) {
        // FIXME: Rethink API.  For now I just need milliseconds, periodic.
        ASSERT(tok = strtok(NULL, delim));
        const char *ms_str = tok;
        long ms = atol(ms_str);
        ASSERT(ms > 0);
        ASSERT(NULL == (tok = strtok(NULL, delim)));
        return port_open_timerfd_stream(ms);
    }

    // -:<framing>
    // stdandard i/o
    if (!strcmp(tok, "-")) {
        tok = strtok(NULL, delim);
        if (NULL == tok) {
            return port_open_sys(0, 1);
        }
        else if (!strcmp("slip", tok)) {
            ASSERT(NULL == (tok = strtok(NULL, delim)));
            return port_open_slip_stream(0, 1);
        }
        else {
            uint16_t len_bytes = atoi(tok);
            ASSERT(NULL == (tok = strtok(NULL, delim)));
            return port_open_packetn_stream(len_bytes, 0, 1);
        }
    }

    if (!strcmp(tok, "HEX")) {
        ASSERT(NULL == (tok = strtok(NULL, delim)));
        return port_open_hex_stream(0, 1);
    }

    if (!strcmp(tok, "LINE")) {
        ASSERT(NULL == (tok = strtok(NULL, delim)));
        return port_open_line_stream(0, 1);
    }

#ifdef HAVE_LIBUSB
    if (!strcmp(tok, "USB")) {
        ASSERT(NULL == (tok = strtok(NULL, delim)));
        return port_open_usb();
    }
#endif


    ERROR("unknown type %s\n", tok);
}


/***** 7. DEFAULT MAIN */

int packet_forward_main(int argc, char **argv) {
    ASSERT(argc > 2);
    struct port *port[2];
    struct packet_handle_ctx ctx = {
        .nb_ports = 2,
        .port = port,
        .timeout = -1 // infinity
    };
    ASSERT(port[0] = port_open(argv[1]));
    ASSERT(port[1] = port_open(argv[2]));
    packet_loop(packet_forward, &ctx);
    return 0;
}



/* To set up UDP someone needs to send a first packet.  All other
   packets will go back to the first peer.  These are the
   configurations:

   udp2udp <port1> <port2> [<dst_host> <dst_port>]

   tap2udp <tap>   <port>  [<dst_host> <dst_port>]

   dst_host, dst_port are optional.  If present, a first packet is
   sent to initate communication.

   It seems that will solve all topologies.

   EDIT: I need to think about this some more.  Problem is that I
   don't understand UDP very well, i.e.: how can I let the OS pick a
   port?  Because in the a-symmetric situation, I want one listening
   port that is well known, but the other port can be arbitrary since
   it will be chained to the next hop, and that one will reply to any
   port that is specified.  I think I get it now, just figure out how
   to do this.  I guess this just boils down to binding or not.

   So what about this: add L: or C: prefixes to set up the initial
   direction of the socket.

   Follow socat suntax:

   TAP:tap0
   UDP4-LISTEN:port
   UDP4:host:port


   Setup:
   - ssh to the endpoint, start udp-listen A + tap
   - ssh to the midpont,  start udp-listen B + udp-connect to A
   - .. other midpoints ..
   - local: start tap + udp-connect to B
   - gather all pids doing so and close ssh connections
   - monitor connection, if it goes down tear down old and rebuild

*/


/* TODO:
   - remove port creation duplication wrt. slip vs. packetn */



