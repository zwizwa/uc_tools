/* To the extent possible under law, Tom Schouten has waived all
   copyright and related or neighboring rights to infof.h
   Code:    http://zwizwa.be/git/uc_tools
   License: http://creativecommons.org/publicdomain/zero/1.0 */

#include "infof.h"
#include <stdarg.h>



void NS(decimal)(NS(CTX_DEF) int d) {
    if (d < 0) { NS(putchar)(NS(CTX_REF) '-'); d = -d; }
    char stack[12]; // enough for max 2^31
    char *s = stack;
    while(d) {*s++ = '0' + d % 10; d /= 10;}
    if (s == stack) NS(putchar)(NS(CTX_REF) '0');
    else while(s > stack) NS(putchar)(NS(CTX_REF) *--s);
}
void NS(hex)(NS(CTX_DEF) unsigned int d, int digits) {
    while(digits > 0) {
        NS(putchar)(NS(CTX_REF) "0123456789abcdef"[(d >> (4*(--digits))) & 0xF]);
    }
}
void NS(hex_u8)(NS(CTX_DEF) const uint8_t *buf, int n) {
    for (int i = 0; i<n; i++) {
        NS(putchar)(NS(CTX_REF) ' ');
        NS(hex)(NS(CTX_REF) buf[i], 2);
    }
}
void NS(hex_u16)(NS(CTX_DEF) const uint16_t *buf, int n) {
    for (int i = 0; i<n; i++) {
        NS(putchar)(NS(CTX_REF) ' ');
        NS(hex)(NS(CTX_REF) buf[i], 4);
    }
}
void NS(str)(NS(CTX_DEF) const char *c) {
    while(*c) NS(putchar)(NS(CTX_REF) *c++);
}
void NS(str_n)(NS(CTX_DEF) const char *c, int n) {
    while(n--) NS(putchar)(NS(CTX_REF) *c++);
}
static inline int NS(is_digit)(int d) {
    return d >= '0' && d <= '9';
}

/* The escape character is abstracted here to allow the valid C
   identifier character '_' to be used by tracef. */
int NS(vf)(NS(CTX_DEF) const char *fmt, va_list ap) {
    while (*fmt) {
        if (*fmt == '%') {
            fmt++;
            /* Ignore '0'; always print leading zeros for %x, by default: %08x.
               For %d all numeric arguments are ignored. */
            int nb_digits = 8;
            while (NS(is_digit)(*fmt)) {nb_digits = *fmt-'0'; fmt++;}
            switch(*fmt) {
            case 0: break;
            case 'p':
            case 'X':
            case 'x': fmt++; NS(hex)(NS(CTX_REF) va_arg(ap,int),nb_digits); break;
            case 'u': // FIXME
            case 'd': fmt++; NS(decimal)(NS(CTX_REF) va_arg(ap, int));      break;
            case 's': fmt++; NS(str)(NS(CTX_REF) va_arg(ap,const char*));   break;
            case 'c': fmt++; NS(putchar)(NS(CTX_REF) va_arg(ap, int));      break;
            /* Nonstandard: raw binary strings as hex. */
            case 'b': fmt++; NS(hex_u8)(NS(CTX_REF) va_arg(ap, uint8_t*), nb_digits); break;
            default:
                NS(putchar)(NS(CTX_REF) '%');
                NS(putchar)(NS(CTX_REF) *fmt);
                fmt++;
                break;
            }
        }
        else {
            NS(putchar)(NS(CTX_REF) *fmt++);
        }
    }
    return 0; // this is not used.
}


