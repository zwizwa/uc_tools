#ifndef PACKET_LOOP_H
#define PACKET_LOOP_H

#include "macros.h"
#include <stdint.h>
#include <unistd.h>

struct packet_loop_state {
    int *fds; // pointer to active file descriptors
    uint32_t nfds;
};

struct packet_loop_config {
    /* Note that buf is explictly not const: it is allowed to be
       modified in-place for further processing. */
    void (*push)(struct packet_loop_state *s, int from_fd,
                 uint8_t *buf, uint32_t len);
    uint32_t packet_size; // total packet size, incl header and 4-byte size
    uint32_t header_size; // amount of space to skip in buffer
    uint16_t tcp_port;
};

void packet_loop_start(const struct packet_loop_config *config);


/* Some simple application push methods. */
static inline void packet_loop_echo_push(
    struct packet_loop_state *s, int in_fd,
    uint8_t *buf, uint32_t len) {

    /* Forward to everone except self. */
    for (int i=0; i<s->nfds; i++) {
        int out_fd = s->fds[i];
        if (in_fd != out_fd) {
            ASSERT(len == write(out_fd, buf, len));
        }
    }
}


#endif
