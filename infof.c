/* To the extent possible under law, Tom Schouten has waived all
   copyright and related or neighboring rights to infof.h
   Code:    http://zwizwa.be/git/uc_tools
   License: http://creativecommons.org/publicdomain/zero/1.0 */

#include "infof.h"
#include <stdarg.h>

void info_decimal(int d) {
    if (d < 0) { info_putchar('-'); d = -d; }
    char stack[12]; // enough for max 2^31
    char *s = stack;
    while(d) {*s++ = '0' + d % 10; d /= 10;}
    if (s == stack) info_putchar('0');
    else while(s > stack) info_putchar(*--s);
}
void info_hex(unsigned int d, int digits) {
    while(digits > 0) {
        info_putchar("0123456789abcdef"[(d >> (4*(--digits))) & 0xF]);
    }
}
void info_hex_u8(const uint8_t *buf, int n) {
    for (int i = 0; i<n; i++) {
        info_putchar(' ');
        info_hex(buf[i], 2);
    }
}
void info_hex_u16(const uint16_t *buf, int n) {
    for (int i = 0; i<n; i++) {
        info_putchar(' ');
        info_hex(buf[i], 4);
    }
}
void info_str(const char *c) {
    while(*c) info_putchar(*c++);
}
void info_str_n(const char *c, int n) {
    while(n--) info_putchar(*c++);
}
static inline int is_digit(int d) {
    return d >= '0' && d <= '9';
}

/* The escape character is abstracted here to allow the valid C
   identifier character '_' to be used by tracef. */
int vinfof_with_escape(char escape, const char *fmt, va_list ap) {
    while (*fmt) {
        if (*fmt == escape) {
            fmt++;
            /* Ignore '0'; always print leading zeros for %x, by default: %08x.
               For %d all numeric arguments are ignored. */
            int nb_digits = 1;
            while (is_digit(*fmt)) {nb_digits = *fmt-'0'; fmt++;}
            switch(*fmt) {
            case 0: break;
            case 'p':
            case 'X':
            case 'x': fmt++; info_hex(va_arg(ap,int),nb_digits); break;
            case 'u': // FIXME
            case 'd': fmt++; info_decimal(va_arg(ap, int));      break;
            case 's': fmt++; info_str(va_arg(ap,const char*));   break;
            case 'c': fmt++; info_putchar(va_arg(ap, int));      break;
            /* Nonstandard: raw binary strings as hex. */
            case 'b': fmt++; info_hex_u8(va_arg(ap, uint8_t*), nb_digits); break;
            /* To print spaces with vtracef. */
            case '_': fmt++; info_putchar(' ');                  break;
            default:
                info_putchar(escape);
                info_putchar(*fmt);
                fmt++;
                break;
            }
        }
        else {
            info_putchar(*fmt++);
        }
    }
    return 0; // this is not used.
}

int vinfof(const char *fmt, va_list ap) {
    return vinfof_with_escape('%', fmt, ap);
}

int infof(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    int rv = vinfof(fmt, ap);
    va_end(ap);
    return rv;
}

