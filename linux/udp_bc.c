/* Unpack the TAG_STREAM (0xFFFB) protocol to UDP broadcast, mapping
   the stream number to UDP port. */

#include "tag_protocol.h"
#include "tcp_tools.h"

#include <stdint.h>
#include <sys/types.h>
#include <netinet/in.h>

int sock_fd;
struct sockaddr_in broadcast_addr;

void udp_bc(uint16_t port, uint8_t *buf, uint32_t len) {
    // LOG("- udp_bc %d %d\n", port, len);
    broadcast_addr.sin_port = htons(port);
    int flags = 0;
    ASSERT_ERRNO(
        sendto(sock_fd, buf, len, flags,
               (struct sockaddr*)&broadcast_addr,
               sizeof(broadcast_addr)));
}

int main(int argc, char **argv) {
    ASSERT(argc >= 2);
    if (argc == 3) {
        // Check second argument for compatibility with packet_bridge.
        // For us the stdin protocol is hardcoded, so make sure its
        // what caller expects.
        ASSERT(!strcmp("-:4",argv[2]));
    }
    const char *bc_addr = argv[1];
    ASSERT_ERRNO(sock_fd = socket(AF_INET, SOCK_DGRAM, IPPROTO_UDP));
    ASSERT_ERRNO(setsockopt(sock_fd, SOL_SOCKET, SO_BROADCAST, &(int){ 1 }, sizeof(int)));
    assert_gethostbyname(&broadcast_addr, bc_addr);

    for(;;) {
        uint32_t len = assert_read_be(0, 4);
        uint16_t tag = assert_read_be(0, 2);
        uint8_t msg[len];
        assert_read(0, msg, len);
        switch(tag) {
        case 0xFFFB: {
            ASSERT(len >= 2);
            uint16_t port = read_be(msg, 2);
            udp_bc(port,msg+2,len-4);
            break;
        }
        default:
            LOG("unknown: tag=0x%04x len=%d\n", tag, len);
        }
    }
}
