#ifndef TOOLS_H
#define TOOLS_H

#include <stdint.h>

uint8_t hex_int2char(uint32_t i);
uint32_t hex_char2int_ignore(uint8_t ch); // ignore invalid (return 0)
int32_t hex_char2int_check(uint8_t ch);   // invalid -> -1

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
static inline uint32_t read_hex_nibbles(const uint8_t *buf, int nibbles) {
    uint32_t word = 0;
    for(int i=0; i<nibbles; i++) {
        word <<= 4;
        word |= 0xF & hex_char2int_ignore(buf[i]);
    }
    return word;
}

static inline int32_t read_hex_nibbles_check(const uint8_t *buf, int nibbles,
                                             uint32_t *pword) {
    uint32_t word = 0;
    for(int i=0; i<nibbles; i++) {
        word <<= 4;
        int rv = hex_char2int_check(buf[i]);
        if (rv < 0) return rv;
        word |= 0xF & rv;
    }
    *pword = word;
    return 0;
}



#endif
