/* Implement a {packet,4} echo server on top of a {packet,4} event loop. */

#include "macros.h"
#include "packet_loop.h"

/* Push receives the packet including the 4-byte size prefix.
   We have requested a header_size of 0, so size prefix is at offset 0.
   This means packet can just be copied raw to to other fd. */
static void push(
    struct packet_loop_state *s, int in_fd,
    uint8_t *buf, uint32_t len) {
    packet_loop_echo_push(s, in_fd, buf, len);
}

int main(int argc, char **argv) {
    ASSERT(argc == 2);
    int port = atoi(argv[1]);
    struct packet_loop_config config = {
        .tcp_port    = port,
        .push        = push,
        .header_size = 0,
        .packet_size = 2048,
    };
    packet_loop_start(&config);
}
