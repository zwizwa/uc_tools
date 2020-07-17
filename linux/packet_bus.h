#ifndef PACKET_BUS_H
#define PACKET_BUS_H

#include <stdint.h>

struct packet_bus_config {
    uint16_t tcp_port;
    void (*sniff)(int fd, const uint8_t *buf, uint32_t len);
};

void packet_bus_start(const struct packet_bus_config *config);

#endif
