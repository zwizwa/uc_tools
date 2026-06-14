#ifndef HW_I686_IO
#define HW_I686_IO

#include <stdint.h>


static inline void log_hex(const uint8_t *buf, uint32_t len) {
    for (int i=0; i<len; i++) {
        if (i % 16 == 0) {
            LOG("%04x ", i);
        }
        LOG(" %02x", buf[i]);
        if (i % 16 == 7) {
           LOG(" ");
        }
        if (i % 16 == 15) {
            LOG("\n");
        }
    }
    if ((len % 16) != 0) {
        LOG("\n");
    }
}



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

// -device isa-debug-exit,iobase=0xf4,iosize=0x04
// qemu exits using exit((code << 1) | 1)
static inline void qemu_exit(uint16_t code) {
    LOG("qemu_exit(%d) -> %d\n", code, (code<<1)|1);
    outw(0xf4, code);
}
static inline void shutdown(void) {
    qemu_exit(0);
}

static inline void cli_and_restart(void) {
    LOG("restart...\n");
    cli();
    __asm__ __volatile__ (
        "movl $0x7C00, %%esp"   "\n\t"
        "movl [0x7E00], %%edx"  "\n\t"
        "jmp *%%edx"            "\n\t"
        : : : "memory"
        );
}






#endif
