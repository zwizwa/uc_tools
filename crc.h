#ifndef CRC_H
#define CRC_H

#include <stdint.h>

// https://stackoverflow.com/questions/21001659/crc32-algorithm-implementation-in-c-without-a-look-up-table-and-with-a-public-li

/* This is the basic CRC-32 calculation with some optimization but no
   table lookup. The the byte reversal is avoided by shifting the crc
   reg right instead of left and by using a reversed 32-bit word to
   represent the polynomial. */

static inline uint32_t crc32b_inc(const uint8_t *buf, uint32_t len, uint32_t crc_init) {
    /* Operate on inverted bits. */
    uint32_t crc = ~crc_init;
    for (int i=0; i<len; i++) {
        crc = crc ^ buf[i];
        for (int j = 7; j >= 0; j--) {
            uint32_t mask = -(crc & 1);
            crc = (crc >> 1) ^ (0xEDB88320 & mask);
        }
    }
    return ~crc;
}
static inline uint32_t crc32b(const uint8_t *buf, uint32_t len) {
    return crc32b_inc(buf, len, 0);
}

// Also plucked from stackoverflow.
// Initialize with 0xff
/* 0x131 x^8 + x^5 + x^4 + 1 */
static inline unsigned crc8x_simple(unsigned crc, void const *mem, uintptr_t len) {
    unsigned char const *data = mem;
    if (!data)
        return 0xff;
    while (len--) {
        crc ^= *data++;
        for (unsigned k = 0; k < 8; k++)
            crc = crc & 0x80 ? (crc << 1) ^ 0x31 : crc << 1;
    }
    crc &= 0xff;
    return crc;
}

// BSD checksum
// https://en.wikipedia.org/wiki/BSD_checksum
// Initialize with state=0
static inline uint16_t bsd_checksum_inc(const uint8_t *buf, uint32_t len, uint16_t state) {
    for(int i=0; i<len; i++) {
        state = (state >> 1) + ((state & 1) << 15);
        state += buf[i];
    }
    return state;
}

/*

Flash checksum:

extern uint8_t _stext;
extern uint8_t _etext;
    uint32_t cs = crc32b(&_stext, &_etext - &_stext);
    infof("S:%08x L:%d CRC:%08x\n", &_stext, &_etext - &_stext, cs);

*/

#endif
