#include "macros.h"
#include "packet_bus.h"

void sniff(int fd, const uint8_t *buf, uint32_t len) {
}

int main(int argc, char **argv) {
    ASSERT(argc == 2);
    int port = atoi(argv[1]);
    struct packet_bus_config config = {
        .tcp_port = port,
        .sniff    = sniff
    };
    packet_bus_start(&config);
}
