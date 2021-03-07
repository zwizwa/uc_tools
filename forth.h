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

/* Not that the encoding was written specificially for Thumb2, which
   has LSB conveniently set to 1 for code pointers.  The "portable"
   workaround below is a terrible hack, only used to run tests and
   perform bootstreapping. */
#ifdef STM32F1
INLINE int xt_is_code(w xt)   { return xt.u & 1; }
INLINE int xt_is_word(w xt) { return (xt.u & 3) == 0; }
#define IOPC(x) ((uintptr_t)(2 | ((x)<<2)))

#else

static inline int xt_is_token(w xt) {
    return xt.u < 2;
}
static void enter(w* list);
static inline int xt_is_word(w xt) {
    if (xt_is_token(xt)) return 0;
    /* If it's not a token, it's at least a valid pointer, so we can
       dereference.  All threaded words currently start with enter, so
       use that to distinguish. */
    return ((uintptr_t)enter) == *(xt.up);
}
static inline int xt_is_code(w xt) {
    return (!xt_is_token(xt)) && (!xt_is_word(xt));
}
#define IOPC(x) ((uintptr_t)x)

#endif



void forth_start(void);

uint32_t forth_read(uint8_t *buf, uint32_t size);
void forth_write(const uint8_t *buf, uint32_t len);

void forth_putchar(int c);
void forth_puts(char *s);

#endif
