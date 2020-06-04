#ifndef BYTESWAP_H
#define BYTESWAP_H

#include <stdint.h>
// ---- BIG ENDIAN

// Buffer read and write
static inline uint64_t read_be(const uint8_t *buf, uint32_t nb) {
    uint64_t accu = 0;
    while(nb) {
        accu = (accu << 8) | (*buf++);
        nb--;
    }
    return accu;
}
static inline void write_be(uint8_t *buf, uint64_t word, uint32_t nb) {
    buf += nb;
    while(nb) {
        *(--buf) = word;
        word = word >> 8;
        nb--;
    }
}

// Array splices with embedded commas
#define U16_BE(t) ((t) >> 8), ((t) & 0xFF)
#define U32_BE(t) (U16_BE((t) >> 16)), U16_BE((t) & 0xFFFF)


// ---- SWAPPERS
//
#define U16_SWAP(x) ((((x)>>8)&0xff) | (((x)&0xff) << 8))
#define U32_SWAP(x) (SWAP_U16((x)>>16) | (SWAP_U16(x)<<16))


#endif
