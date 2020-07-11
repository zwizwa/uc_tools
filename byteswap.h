#ifndef BYTESWAP_H
#define BYTESWAP_H

#include <stdint.h>

/* Byte order swappers Please do not use the swappers directly.  Use
   ntoh and hton variants that are conditional on host endianness. */

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
/* Array splices with embedded commas */
#define U16_BE(t) ((t) >> 8), ((t) & 0xFF)
#define U32_BE(t) U16_BE((t) >> 16), U16_BE((t) & 0xFFFF)




/* These operate in-place.  Most of the time the original network
   order buffer does not need to be retained. */
static inline void byteswap_field(uint8_t *buf, uint32_t len) {
    for(uint32_t i=0; i<len/2; i++) {
        uint8_t a = buf[i];
        uint8_t b = buf[len-1-i];
        buf[i] = b;
        buf[len-1-i] = a;
    }
}
struct byteswap_field {
    /* Dare I use bitfields? */
    int len:15;
    int swap:1;
} __attribute__((__packed__));
static inline void byteswap_fields(const struct byteswap_field *fields, uint32_t nb_fields, void *vbuf) {
    uint8_t *buf = vbuf;
    for(uint32_t i=0; i<nb_fields; i++,fields++) {
        if (fields->swap) {
            byteswap_field(buf, fields->len);
        }
        buf += fields->len;
    }
}



#define SWAP_U16(x) ((((x)>>8)&0xff) | (((x)&0xff) << 8))
#define SWAP_U32(x) (SWAP_U16((x)>>16) | (SWAP_U16(x)<<16))

#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#define HTONS(s) (s)
#define NTOHS(s) (s)
#define HTONL(l) (l)
#define NTOHL(l) (l)
static inline void ntoh_fields(const struct byteswap_field *fields, uint32_t nb_fields, void *buf) {}
static inline void hton_fields(const struct byteswap_field *fields, uint32_t nb_fields, void *buf) {}

#elif __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define HTONS(s) SWAP_U16(s)
#define NTOHS(s) SWAP_U16(s)
#define HTONL(l) SWAP_U32(l)
#define NTOHL(l) SWAP_U32(l)
static inline void hton_fields(const struct byteswap_field *fields, uint32_t nb_fields, void *buf) {
    byteswap_fields(fields, nb_fields, buf);
}
static inline void ntoh_fields(const struct byteswap_field *fields, uint32_t nb_fields, void *buf) {
    byteswap_fields(fields, nb_fields, buf);
}

#else
#error "Can't determine endianness"
#endif





#endif
