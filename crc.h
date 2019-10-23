#ifndef CRC_H
#define CRC_H

#include <stdint.h>

// https://stackoverflow.com/questions/21001659/crc32-algorithm-implementation-in-c-without-a-look-up-table-and-with-a-public-li

/* This is the basic CRC-32 calculation with some optimization but no
   table lookup. The the byte reversal is avoided by shifting the crc
   reg right instead of left and by using a reversed 32-bit word to
   represent the polynomial. */

uint32_t crc32(const uint8_t *buf, uint32_t len) {
    uint32_t crc = 0xFFFFFFFF;
    for (int i=0; i<len; i++) {
        crc = crc ^ buf[i];
        for (int j = 7; j >= 0; j--) {
            uint32_t mask = -(crc & 1);
            crc = (crc >> 1) ^ (0xEDB88320 & mask);
        }
    }
    return ~crc;
}


#endif
