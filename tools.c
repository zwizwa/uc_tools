#include "tools.h"

uint8_t hex_int2char(uint32_t i) {
    if (i>15) i = 16;
    return "0123456789abcdefX"[i];
}
int32_t hex_char2int_check(uint8_t ch) {
    if ((ch >= '0') && (ch <= '9')) return ch - '0';
    if ((ch >= 'a') && (ch <= 'f')) return ch - 'a' + 10;
    if ((ch >= 'A') && (ch <= 'F')) return ch - 'A' + 10;
    return -1;
}
int32_t dec_char2int_check(uint8_t ch) {
    if ((ch >= '0') && (ch <= '9')) return ch - '0';
    return -1;
}

uint32_t hex_char2int_ignore(uint8_t ch) {
    int32_t d = hex_char2int_check(ch);
    if (d<0) d = 0;
    return d;
}
uint32_t read_hex_byte(const uint8_t *buf) {
    return read_hex_nibbles(buf, 2);
}
uint32_t read_hex_u32_le(const uint8_t *c) {
    return (read_hex_byte(c))
        |  (read_hex_byte(c+2) << 8)
        |  (read_hex_byte(c+4) << 16)
        |  (read_hex_byte(c+6) << 24);
}
void bin_to_hex(const uint8_t *in, uint32_t nb_in, uint8_t *hex_out) {
    for (int i = 0; i < nb_in; i++) {
        write_hex_nibbles(hex_out + 2*i, in[i], 2);
    }
    hex_out[2*nb_in] = '\n';
    hex_out[2*nb_in+1] = 0;
}
int hex_to_bin(const uint8_t *in_hex, uint8_t *buf, uint32_t size) {
    uint32_t nb_in = 0;
    for (int i = 0; in_hex[i]; i+=2) {
        if (nb_in >= size) return -1;
        buf[nb_in] = read_hex_byte(in_hex+i);
        nb_in++;
    }
    return nb_in;
}

