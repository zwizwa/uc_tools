#ifndef PACKET_LOOP_H
#define PACKET_LOOP_H

#include "macros.h"
#include <stdint.h>
#include <unistd.h>
#include <string.h>

struct packet_loop_state {
    int *fds; // pointer to active file descriptors
    uint32_t nfds;
};

struct packet_loop_config {
    void (*notify)(struct packet_loop_state *s, int from_fd, int state);

    /* Note that buf is explictly not const: it is allowed to be
       modified in-place for further processing. */
    void (*push)(struct packet_loop_state *s, int from_fd, uint8_t *buf, uint32_t len);
    uint32_t packet_size; // total packet size, incl header and 4-byte size
    uint32_t header_size; // amount of space to skip in buffer
    uint16_t tcp_port;
};

void packet_loop_start(const struct packet_loop_config *config);



static inline void packet_loop_forward(int out_fd, const uint8_t *buf, uint32_t len) {
    //LOG("- packet_loop_forward %d\n", out_fd);
    int left = len;
    while (left) {
        int rv = write(out_fd, buf + (len-left), left);
        if (-1 == rv) {
            if (ECONNRESET == errno) {
                /* Disconnect is not an error. */
                return;
            }
            else {
                /* Log other conditions but don't treat them as errors. */
                const char *e = strerror(errno);
                LOG("WARNING: packet_loop_echo_push: out_fd=%d, errno=%d, %s\n", out_fd, errno, e);
                return;
            }
        }
        ASSERT(rv > 0);
        left -= rv;
    }
}


/* Some simple application push methods. */
static inline void packet_loop_echo_push(
    struct packet_loop_state *s, int in_fd,
    const uint8_t *buf, uint32_t len) {

    //LOG("packet_loop_echo_push %d\n", in_fd);

    /* Forward to everone except self. */
    for (int i=0; i<s->nfds; i++) {
        int out_fd = s->fds[i];
        if (in_fd != out_fd) {
            packet_loop_forward(out_fd, buf, len);
        }
    }
}


#endif
