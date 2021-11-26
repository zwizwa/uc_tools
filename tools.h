#ifndef TOOLS_H
#define TOOLS_H

#include <stdint.h>

uint8_t hex_int2char(uint32_t i);
uint32_t hex_char2int_ignore(uint8_t ch); // ignore invalid (return 0)
int32_t hex_char2int_check(uint8_t ch);   // invalid -> -1

int32_t dec_char2int_check(uint8_t ch);   // invalid -> -1

void bin_to_hex(const uint8_t *in, uint32_t nb_in, uint8_t *hex_out);
int hex_to_bin(const uint8_t *in_hex, uint8_t *buf, uint32_t size);

uint32_t read_hex_byte(const uint8_t *c);
uint32_t read_hex_u32_le(const uint8_t *c);

static inline void write_hex_nibbles(uint8_t *buf, uint32_t word, int nibbles) {
    for(int i=0; i<nibbles; i++) {
        int shift = 4 * (nibbles - 1 - i);
        buf[i] = hex_int2char((word >> shift) & 0xF);
    }
}
static inline uint32_t read_hex_nibbles(const uint8_t *hex_buf, int nibbles) {
    uint32_t word = 0;
    for(int i=0; i<nibbles; i++) {
        word <<= 4;
        word |= 0xF & hex_char2int_ignore(hex_buf[i]);
    }
    return word;
}

static inline int32_t read_hex_nibbles_check(const uint8_t *hex_buf, int nibbles,
                                             uint32_t *pword) {
    uint32_t word = 0;
    for(int i=0; i<nibbles; i++) {
        word <<= 4;
        int rv = hex_char2int_check(hex_buf[i]);
        if (rv < 0) return rv;
        word |= 0xF & rv;
    }
    *pword = word;
    return 0;
}

static inline int32_t read_hex_to_bytes_check(
    const uint8_t *hex_buf, uint8_t *pbytes, uint32_t nb_bytes) {
    for (uint32_t i=0; i<nb_bytes; i++) {
        int32_t rv;
        uint32_t w;
        if (0 != (rv = read_hex_nibbles_check(hex_buf + 2*i, 2, &w))) return rv;
        pbytes[i] = w;
    }
    return 0;
}

static inline int32_t read_hex_nibbles_check_uptr(const uint8_t *buf, int nibbles,
                                                  uintptr_t *pword) {
    uintptr_t word = 0;
    for(int i=0; i<nibbles; i++) {
        word <<= 4;
        int rv = hex_char2int_check(buf[i]);
        if (rv < 0) return rv;
        word |= 0xF & rv;
    }
    *pword = word;
    return 0;
}

static inline int32_t read_dec_nibbles_check_uptr(const uint8_t *buf, int nibbles,
                                                  uintptr_t *pword) {
    uintptr_t word = 0;
    for(int i=0; i<nibbles; i++) {
        word *= 10;
        int rv = dec_char2int_check(buf[i]);
        if (rv < 0) return rv;
        word += rv; // Trust this is 0-9
    }
    *pword = word;
    return 0;
}



/* Read bits from an arbitrary length word stored as a uint32_t array
   in little endian foramt.  This is used for reading SD registers --
   which are big endian so need a swap operation when stored.

   Prototype: same as SD spec list:
   bitfields listed as end:start (non-exclusive end) */

static inline uint32_t bitslice(uint32_t *words, uint32_t end, uint32_t start) {
    uint32_t endx = end+1;
    uint32_t i = start >> 5;
    uint32_t s = start & 31;
    uint32_t mask = (1 << (endx-start)) - 1;
    return ((words[i] >> s) | words[i+1] << (32-s)) & mask;
}



/* Replacement strcmp, as the one from libc is large. */
static inline int mini_strcmp(const char *s1, const char *s2) {
    const uint8_t *p1 = (const uint8_t *)s1;
    const uint8_t *p2 = (const uint8_t *)s2;
    while (*p1 && *p1 == *p2) { ++p1; ++p2; }
    return (*p1 > *p2) - (*p2 > *p1);
}
static inline char *mini_strcpy(char *dst, const char *src) {
    while(*src) { *dst++ = *src++; }
    return dst;
}
static inline void *mini_memcpy(void *dest, const void *src, uintptr_t n) {
    uint8_t *d = dest;
    const uint8_t *s = src;
    for (uintptr_t i=0; i<n; i++) { *d++ = *s++; };
    return dest;
}
static inline void *mini_memset(void *s, int c, uintptr_t n) {
    uint8_t *p = s;
    for (uintptr_t i=0; i<n; i++) { *p++ = c; };
    return s;
}


/* Round to the next power of two. */
static inline uint32_t aligned_block(uint32_t log2, uint32_t val) {
    return ((val-1)>>log2)+1;
}
static inline uint32_t aligned_bytes(uint32_t val, uint32_t log2) {
    return aligned_block(log2, val) << log2;
}


#endif
