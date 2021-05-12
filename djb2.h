#ifndef DJB2_H
#define DJB2_H

/* This algorithm (k=33) was first reported by dan bernstein many
 * years ago in comp.lang.c. another version of this algorithm (now
 * favored by bernstein) uses xor: hash(i) = hash(i - 1) * 33 ^
 * str[i]; the magic of number 33 (why it works better than many other
 * constants, prime or not) has never been adequately explained.
 *
 * http://www.cse.yorku.ca/~oz/hash.html
 */
#include <stdint.h>
static inline __attribute__((always_inline)) uint32_t djb2_add(const char *str) {
    uint32_t hash = 5381;
    int c;
    while (c = *str++) {
        hash = ((hash << 5) + hash) + c; /* hash * 33 + c */
    }
    return hash;
}

static inline __attribute__((always_inline)) uint32_t djb2_xor(const char *str) {
    uint32_t hash = 5381;
    uint32_t c;
    while (c = *str++) {
        hash = ((hash << 5) + hash) ^ c; /* hash * 33 ^ c */
    }
    return hash;
}

#endif
