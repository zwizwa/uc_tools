/* To the extent possible under law, Tom Schouten has waived all
   copyright and related or neighboring rights to infof.h
   Code:    http://zwizwa.be/git/uc_tools
   License: http://creativecommons.org/publicdomain/zero/1.0 */

#ifndef INFOF_H
#define INFOF_H

#include <stdint.h>

/* infof() is a simplistic version of printf, only depends on
   info_putchar().  The itch this scratches is to enable printf-style
   debugging on an embedded target that does not want a malloc()
   dependency due to printf's dependency on buffered i/o. */

/* Externally defined. */
int info_putchar(int c);
int info_flushed(void);

/* Entry points */
int infof(const char *fmt, ...);
void info_decimal(int d);
void info_hex(unsigned int d, int digits);
void info_hex_u8(const uint8_t *buf, int n);
void info_hex_u16(const uint16_t *buf, int n);
void info_str(const char *c);
void info_str_n(const char *c, int n);


static inline void info_block_data(uint32_t block, uint8_t *data, uint32_t block_size) {
    infof("block %d\n", block);
    for (int i = 0; i<block_size; i+=32) {
        info_hex_u8(data + i, 32);
        info_putchar('\n');
    }
}

#endif


