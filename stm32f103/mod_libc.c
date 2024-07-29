#ifndef MOD_LIBC
#define MOD_LIBC

#include <stddef.h>
#include "tools.h"

/* Smaller re-implementations of libc functions.  Include these in the
   main image to override libc. */

void *memcpy(void *dest, const void *src, size_t n) {
    return mini_memcpy(dest, src, n);
}
int strcmp(const char *s1, const char *s2) {
    return mini_strcmp(s1, s2);
}
char *strcpy(char *dst, const char *src) {
    return mini_strcpy(dst, src);
}
void *memset(void *s, int c, size_t n) {
    return mini_memset(s, c, n);
}

#endif
