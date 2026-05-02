#ifndef HW_I686_CRTC
#define HW_I686_CRTC

#include "hw_i686_io.h"

// Kernel assumes color mode is active.
#define CRTC_ADDR 0x3D4 // CRTC Address Register (also called the CRTC Index Register)
#define CRTC_DATA 0x3D5 // CRTC Data Register
#define CRTC_CURSOR_LOC_HI 0x0E
#define CRTC_CURSOR_LOC_LO 0x0F

static inline void crtc_write(uint8_t reg, uint8_t val) {
    outb(CRTC_ADDR, reg);
    outb(CRTC_DATA, val);
}
static inline uint8_t crtc_read(uint8_t reg) {
    outb(CRTC_ADDR, reg);
    return inb(CRTC_DATA);
}
static inline uint16_t get_cursor_pos(void) {
    uint16_t pos = crtc_read(CRTC_CURSOR_LOC_LO);
    pos |= ((uint16_t)crtc_read(CRTC_CURSOR_LOC_HI)) << 8;
    return pos;
}

static inline void top_left(uint32_t word, int nb_digits) {
    volatile uint8_t *v = (void*)0xB8000;
    static const uint8_t hex[16] = "0123456789ABCDEF";
    for (int i=nb_digits-1; i>=0; i--) {
        *v++ = hex[(word >> (i*4)) & 0xF];
        *v++ = 0x17;
    }
}
static inline void set_cursor_pos(uint16_t pos) {
    crtc_write(CRTC_CURSOR_LOC_LO, pos & 0xFF);
    crtc_write(CRTC_CURSOR_LOC_HI, (pos >> 8) & 0xFF);
    // top_left(pos,4);
}


#endif
