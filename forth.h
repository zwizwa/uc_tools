#ifndef FORTH_H
#define FORTH_H

#include "stdint.h"

union word;
typedef void (*code_fn)(union word *);
typedef void (*void_fn)(void);

union word {
    int i;
    /* Changed to uintptr to make it run on 64 bit. */
    uintptr_t u;
    uintptr_t *up;
    uint8_t b;
    char c;
    code_fn code;
    void_fn vcode;
    union word *pw;
    const union word *cpw;
};
typedef union word w;


static inline uintptr_t word_tag(w w) {
    /* ARM: */
    return w.u & 3;
}


void forth_start(void);

uint32_t forth_read(uint8_t *buf, uint32_t size);
void forth_write(const uint8_t *buf, uint32_t len);

void forth_putchar(int c);
void forth_puts(char *s);

#endif
