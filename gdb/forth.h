#ifndef FORTH_H
#define FORTH_H

union word;
typedef void (*code_fn)(union word *);
typedef void (*void_fn)(void);
union word {
    int i;
    uint32_t u32;
    uint32_t *u32p;
    uint8_t b;
    char c;
    code_fn code;
    void_fn vcode;
    union word *pw;
    const union word *cpw;
};
typedef union word w;

void forth_start(void);

uint32_t forth_read(uint8_t *buf, uint32_t size);
void forth_write(const uint8_t *buf, uint32_t len);

#endif
