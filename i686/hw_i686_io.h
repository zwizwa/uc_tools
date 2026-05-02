#ifndef HW_I686_IO
#define HW_I686_IO

#include <stdint.h>

// https://flint.cs.yale.edu/cs422/doc/art-of-asm/pdf/

// https://claude.ai/chat/4749c67f-e209-4871-a53e-3474ed0adf94

// Read a byte from an I/O port
static inline uint8_t inb(uint16_t port) {
    uint8_t val;
    __asm__ volatile ("inb %1, %0" : "=a"(val) : "Nd"(port));
    return val;
}

// Write a byte to an I/O port
static inline void outb(uint16_t port, uint8_t val) {
    __asm__ volatile ("outb %0, %1" : : "a"(val), "Nd"(port));
}

static inline void outw(uint16_t port, uint16_t val) {
    __asm__ volatile ("outw %0, %w1"
        :
        : "a"(val), "Nd"(port)
        : );
}
static inline uint16_t inw(uint16_t port) {
    uint16_t val;
    __asm__ volatile ("inw %w1, %0"
        : "=a"(val)
        : "Nd"(port)
        : );
    return val;
}


static inline void outl(uint16_t port, uint32_t val) {
    __asm__ volatile ("outl %0, %w1"
        : 
        : "a"(val), "Nd"(port)
        : );
}
static inline uint32_t inl(uint16_t port) {
    uint32_t val;
    __asm__ volatile ("inl %w1, %0"
        : "=a"(val)
        : "Nd"(port)
        : );
    return val;
}



static inline void hlt(void) {
    __asm__ volatile ("hlt");
}

static inline void sti(void) {
    __asm__ volatile ("sti");
}

static inline void cli(void) {
    __asm__ volatile ("cli");
}
static inline int interrupts_enabled(void) {
    unsigned long flags;
    asm volatile ("pushfl; popl %0" : "=r"(flags) :: "memory");
    return (flags >> 9) & 1;
}


/* Define this such that qemu does not need to busy-loop. */
static inline void halt(void) {
    cli();
  again:
    hlt();
    goto again;
}

static inline void reboot(void) {
    LOG("reboot...\n");
    outb(0x64, 0xFE);
}



#endif
