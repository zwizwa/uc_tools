#ifndef XMODEM_H
#define XMODEM_H

#include <stdint.h>

#define XMODEM_SOH 0x01
#define XMODEM_STX 0x02
#define XMODEM_EOT 0x04
#define XMODEM_ACK 0x06
#define XMODEM_NAK 0x15
#define XMODEM_CAN 0x18
#define XMODEM_CRC 'C'


static inline uint16_t crc16_xmodem(const uint8_t *p, size_t n) {
    uint16_t crc = 0;
    while (n--) {
        crc ^= (uint16_t)*p++ << 8;
        for (int i = 0; i < 8; i++)
            crc = (crc & 0x8000) ? (crc << 1) ^ 0x1021 : crc << 1;
    }
    return crc;
}

#endif
