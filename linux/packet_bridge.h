/*
Copyright 2019 (c) Tom Schouten

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

#ifndef PACKET_BRIDGE_H
#define PACKET_BRIDGE_H

#include <stdint.h>
#include <sys/types.h>
#include <netinet/in.h>

// Port read/write access and instantiation is abstract
struct port;
struct buf_port;
typedef ssize_t (*port_read_fn)(struct port *, uint8_t *, ssize_t);
typedef ssize_t (*port_write_fn)(struct port *, const uint8_t *, ssize_t);
typedef ssize_t (*port_pop_fn)(struct buf_port *p, uint8_t *buf, ssize_t len);

struct port {
    int fd;              // main file descriptor
    int fd_out;          // optional, if different from main fd
    short events;
    int verbose:1;
    port_read_fn read;
    port_write_fn write;
    port_pop_fn pop;     // only for buffered ports
    const char *spec;
};

struct udp_port {
    struct port p;
    struct sockaddr_in peer;
};

struct port *port_open_tap(const char *dev);
struct port *port_open_timerfd_stream(long ms);
struct port *port_open_udp(uint16_t port);
struct port *port_open_packetn_stream(uint32_t len_bytes, int fd, int fd_out);
struct port *port_open_packetn_tty(uint32_t len_bytes, const char *dev);
struct port *port_open_slip_stream(int fd, int fd_out);
struct port *port_open_slip_tty(const char *dev);
struct port *port_open_hex_stream(int fd, int fd_out);
struct port *port_open_sys(int fd, int fd_out);
struct port *port_open(const char *spec);


// Packet handling is abstracted.  We provide the mainloop, and the
// application provides the handler and port instantiation code.
struct packet_handle_ctx {
    int nb_ports;
    struct port **port;
    int timeout;
};
typedef void (*packet_handle_fn)(struct packet_handle_ctx *, int src, const uint8_t *, ssize_t);
void packet_loop(packet_handle_fn forward, struct packet_handle_ctx *ctx);


// As an example, we provide a handler and instantiator that performs
// simple forwarding between two packet ports.
void packet_forward(struct packet_handle_ctx *, int from, const uint8_t *buf, ssize_t len);
int packet_forward_main(int argc, char **argv);

// Synchronous blocking calls are supported on a single port at a
// time.  This is useful for implementing blocking RPC with timeout.
ssize_t packet_next(struct port *p, int timeout,
                    uint8_t *buf, ssize_t buf_size);

// FIXME: remove static limits here
#define PACKET_BRIDGE_MAX_PACKET_SIZE 4096
#define PACKET_BRIDGE_MAX_EXEC_ARGC 10


#endif
