#ifndef BYTESWAP_H
#define BYTESWAP_H

#include <stdint.h>
#include <string.h>

#include "macros.h"

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
static inline void byteswap_swap(uint8_t *buf, uint32_t len) {
    for(uint32_t i=0; i<len/2; i++) {
        uint8_t a = buf[i];
        uint8_t b = buf[len-1-i];
        buf[i] = b;
        buf[len-1-i] = a;
    }
}
static inline void byteswap_swap_copy(uint8_t *out_buf, const uint8_t *in_buf, uint32_t len) {
    for(uint32_t i=0; i<len/2; i++) {
        uint8_t a = in_buf[i];
        uint8_t b = in_buf[len-1-i];
        out_buf[i] = b;
        out_buf[len-1-i] = a;
    }
}
/* FIXME: Not sure what to take for sizes here.  There is some code
 * that depends on the layout of this struct, so do not change.
 * Create additional functionality instead. */
#if 1
struct byteswap_field {
    uint8_t len;
    uint8_t swap;
} __attribute__((__packed__));
#else
struct byteswap_field {
    unsigned int len:15;
    unsigned int swap:1;
} __attribute__((__packed__));
#endif

CT_ASSERT_STRUCT_SIZE(byteswap_field, 2);

/* Fields is null-terminated (len==0 and swap is ingored).  That is
   more memory effcient and much more convenient for code generators.
   If buffer is NULL then this is run for side effect only, which is
   to compute the total size of the structs.  An explicit function is
   used for that case to make code more readable. */
static inline uint32_t byteswap_fields(const struct byteswap_field *fields, void *buf0) {
    uint8_t *buf = buf0;
    for(uint32_t i=0; fields->len; i++,fields++) {
        if (fields->swap) {
            if (buf0) byteswap_swap(buf, fields->len);
        }
        buf += fields->len;
    }
    return buf - ((uint8_t*)buf0);
}
static inline uint32_t byteswap_fields_total_size(const struct byteswap_field *fields) {
    return byteswap_fields(fields, 0);
}

/* Same but out-of-place. */
static inline uint32_t byteswap_fields_copy(const struct byteswap_field *fields, void *out_buf0, const void *in_buf0) {
    const uint8_t *in_buf = in_buf0;
    uint8_t *out_buf = out_buf0;
    for(uint32_t i=0; fields->len; i++,fields++) {
        if (fields->swap) {
            if (in_buf0) byteswap_swap_copy(out_buf, in_buf, fields->len);
        }
        else {
            if (in_buf0) memcpy(out_buf, in_buf, fields->len);
        }
        in_buf  += fields->len;
        out_buf += fields->len;
    }
    return out_buf - ((uint8_t*)out_buf0);
}


#define BYTESWAP_FIELD(t,s) { .len = sizeof(t), .swap = s }



#define SWAP_U16(x) ((((x)>>8)&0xff) | (((x)&0xff) << 8))
#define SWAP_U32(x) (SWAP_U16((x)>>16) | (SWAP_U16(x)<<16))

#if __BYTE_ORDER__ == __ORDER_BIG_ENDIAN__
#define HTONS(s) (s)
#define NTOHS(s) (s)
#define HTONL(l) (l)
#define NTOHL(l) (l)
#if 0
/* It seems more convenient to just use swap, and dispatch on
 * endianness much higher up the chain for generated byte swappers. */
static inline uint32_t hton_fields(const struct byteswap_field *fields, void *buf) {
    return byteswap_skip_fields(fields, nb_fields, buf);
}
static inline uint32_t ntoh_fields(const struct byteswap_field *fields, void *buf) {
    return byteswap_skip_fields(fields, buf);
}
#endif

#elif __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
#define HTONS(s) SWAP_U16(s)
#define NTOHS(s) SWAP_U16(s)
#define HTONL(l) SWAP_U32(l)
#define NTOHL(l) SWAP_U32(l)
#if 0
static inline uint32_t hton_fields(const struct byteswap_field *fields, void *buf) {
    return byteswap_fields(fields, buf);
}
static inline uint32_t ntoh_fields(const struct byteswap_field *fields, void *buf) {
    return byteswap_fields(fields, buf);
}
#endif

#else
#error "Can't determine endianness"
#endif





#endif
