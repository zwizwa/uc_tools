#ifndef BASE64_H
#define BASE64_H

#include <stdint.h>
#include <stdlib.h>

// From stackoverflow
static inline size_t base64_encoded_length(uintptr_t input_length) {
    return 4 * ((input_length + 2) / 3);
}
// FIXME: Remove this wrapper.
static inline size_t base64_length(uintptr_t input_length) {
    return base64_encoded_length(input_length);
}
static inline void base64_encode(
    char *encoded_data,
    const unsigned char *data,
    size_t input_length) {

    uintptr_t output_length = base64_length(input_length);

    static const char encoding_table[] = {
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
        'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
        'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
        'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
        'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
        'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
        'w', 'x', 'y', 'z', '0', '1', '2', '3',
        '4', '5', '6', '7', '8', '9', '+', '/'};

    for (size_t i = 0, j = 0; i < input_length;) {

        uint32_t octet_a = i < input_length ? (unsigned char)data[i++] : 0;
        uint32_t octet_b = i < input_length ? (unsigned char)data[i++] : 0;
        uint32_t octet_c = i < input_length ? (unsigned char)data[i++] : 0;

        uint32_t triple = (octet_a << 0x10) + (octet_b << 0x08) + octet_c;

        encoded_data[j++] = encoding_table[(triple >> 3 * 6) & 0x3F];
        encoded_data[j++] = encoding_table[(triple >> 2 * 6) & 0x3F];
        encoded_data[j++] = encoding_table[(triple >> 1 * 6) & 0x3F];
        encoded_data[j++] = encoding_table[(triple >> 0 * 6) & 0x3F];
    }

    const int mod_table[] = {0, 2, 1};
    for (int i = 0; i < mod_table[input_length % 3]; i++) {
        encoded_data[output_length - 1 - i] = '=';
    }

}

// Below is from https://claude.ai/chat/32344840-b963-4b37-ae88-5a90fcc857b3
// See also test_base64.c

/* Upper bound on decoded size for an encoded input of input_length chars.
   Exact if input is valid, padded base64. Returns 0 if length is not a
   multiple of 4 (invalid). */
static inline size_t base64_decoded_length(const char *data, size_t input_length) {
    if (input_length % 4 != 0) return 0;
    if (input_length == 0) return 0;
    size_t len = input_length / 4 * 3;
    if (data[input_length - 1] == '=') len--;
    if (data[input_length - 2] == '=') len--;
    return len;
}

/* Decodes standard (RFC 4648) padded base64.
   Writes up to base64_decoded_length() bytes into decoded_data.
   On success returns 0 and sets *output_length (may be NULL).
   Returns -1 on invalid input (bad length, bad char, misplaced '='). */
static inline int base64_decode(
    uint8_t *decoded_data,
    const char *data,
    size_t input_length,
    size_t *output_length) {
    /* Map one base64 character to its 6-bit value.
       Returns -1 for invalid characters, -2 for '=' padding. */
    #define B64_SYM(c) ( \
        ((c) >= 'A' && (c) <= 'Z') ? (c) - 'A'       : \
        ((c) >= 'a' && (c) <= 'z') ? (c) - 'a' + 26  : \
        ((c) >= '0' && (c) <= '9') ? (c) - '0' + 52  : \
        (c) == '+' ? 62 : (c) == '/' ? 63 :            \
        (c) == '=' ? -2 : -1)

    if (input_length % 4 != 0) return -1;
    if (output_length) *output_length = 0;
    if (input_length == 0) return 0;

    size_t j = 0;
    for (size_t i = 0; i < input_length; i += 4) {
        int s0 = B64_SYM((unsigned char)data[i + 0]);
        int s1 = B64_SYM((unsigned char)data[i + 1]);
        int s2 = B64_SYM((unsigned char)data[i + 2]);
        int s3 = B64_SYM((unsigned char)data[i + 3]);
        int last = (i + 4 == input_length);

        /* First two symbols of a quantum can never be padding. */
        if (s0 < 0 || s1 < 0) return -1;
        /* '=' only allowed in the final quantum; "x===" is invalid. */
        if (s2 == -2 && (!last || s3 != -2)) return -1;
        if (s3 == -2 && !last) return -1;
        if (s2 == -1 || s3 == -1) return -1;

        uint32_t triple = ((uint32_t)s0 << 18) | ((uint32_t)s1 << 12) |
                          ((s2 == -2 ? 0u : (uint32_t)s2) << 6) |
                          (s3 == -2 ? 0u : (uint32_t)s3);

        decoded_data[j++] = (unsigned char)((triple >> 16) & 0xFF);
        if (s2 != -2) decoded_data[j++] = (unsigned char)((triple >> 8) & 0xFF);
        if (s3 != -2) decoded_data[j++] = (unsigned char)(triple & 0xFF);
    }
    if (output_length) { *output_length = j; }
    return 0;
    #undef B64_SYM
}


#endif

