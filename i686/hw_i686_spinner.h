#ifndef SPINNER_H
#define SPINNER_H

#include <stdint.h>

/* Simple debug spinner.
   count: only the lower 3 bits are used for the spinner state
   pos:   0 = right-most, 1 = left of that, etc..
*/

static inline void spinner(uint32_t index, uint32_t count) {
    static const uint8_t progress[8] = {'-','\\','|','/','-','\\','|','/'};
    volatile uint8_t *v = (void*)(0xB8000 + 2*index);
    *v++ = progress[count & 7];
    *v++ = 0x17;
}


#endif
