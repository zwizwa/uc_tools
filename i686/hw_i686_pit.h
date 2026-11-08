/* 8254 PIT channel 0 as periodic tick on IRQ0.
   https://wiki.osdev.org/Programmable_Interval_Timer */

#ifndef HW_I686_PIT_H
#define HW_I686_PIT_H

#include "hw_i686_io.h"

#define PIT_HZ 1193182

/* Rate generator, mode 2. */
static inline void pit_init(uint32_t hz) {
    uint16_t div = PIT_HZ / hz;
    outb(0x43, 0x34);  // channel 0, lo/hi byte, mode 2
    outb(0x40, div);
    outb(0x40, div >> 8);
}

#endif
