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
#define U32_BE(t) U16_BE((t) >> 16), U16_BE((t) & 0xFFFF)



// Byte order swappers
//
// Please do not use these two.  Use the HTONS... macros below

#define SWAP_U16(x) ((((x)>>8)&0xff) | (((x)&0xff) << 8))
#define SWAP_U32(x) (SWAP_U16((x)>>16) | (SWAP_U16(x)<<16))

#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#define HTONS(s) (s)
#define NTOHS(s) (s)
#define HTONL(l) (l)
#define NTOHL(l) (l)
#elif __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define HTONS(s) SWAP_U16(s)
#define NTOHS(s) SWAP_U16(s)
#define HTONL(l) SWAP_U32(l)
#define NTOHL(l) SWAP_U32(l)
#else
#error "Can't determine endianness"
#endif




#endif
