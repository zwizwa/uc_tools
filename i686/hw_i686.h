#ifndef HW_I686_H
#define HW_I686_H

#include <stdint.h>

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

static inline void hlt(void) {
    __asm__ volatile ("hlt");
}

static inline void sti(void) {
    __asm__ volatile ("sti");
}

static inline void cli(void) {
    __asm__ volatile ("cli");
}


// Serial port
// https://claude.ai/chat/c5b674e0-3495-485e-ad01-2746fbd8b669

/* COM1 I/O ports */
#define COM1_BASE   0x3F8
#define COM1_DATA   (COM1_BASE + 0)  /* RBR: Receive Buffer Register */
#define COM1_IER    (COM1_BASE + 1)  /* Interrupt Enable Register */
#define COM1_FCR    (COM1_BASE + 2)  /* FIFO Control Register */
#define COM1_LCR    (COM1_BASE + 3)  /* Line Control Register */
#define COM1_MCR    (COM1_BASE + 4)  /* Modem Control Register */
#define COM1_LSR    (COM1_BASE + 5)  /* Line Status Register */

/* LSR bits */
#define LSR_DATA_READY   0x01
#define LSR_OVERRUN_ERR  0x02
#define LSR_PARITY_ERR   0x04
#define LSR_FRAMING_ERR  0x08
#define LSR_THR_EMPTY    0x20  /* Transmitter Holding Register empty */
#define LSR_TX_IDLE      0x40  /* Transmitter fully idle (THR + shift reg) */

static void com1_putchar(uint8_t byte) {
    while (!(inb(COM1_LSR) & LSR_THR_EMPTY)) { }
    outb(COM1_DATA, byte);
}
static void com1_putstr(uint8_t *str) {
    while(*str) {
        com1_putchar(*str++);
    }
}
static void com1_init(void) {
    /* DTR, RTS, OUT2 (OUT2 gates IRQs on PCs) */
    outb(COM1_MCR, 0x0B);
    /* enable "Received Data Available" interrupt */
    outb(COM1_IER, 0x01);
}

// --- IDT Entry (Gate Descriptor) ---
struct idt_entry {
    uint16_t base_low;    // lower 16 bits of ISR address
    uint16_t selector;    // kernel code segment selector (typically 0x08)
    uint8_t  zero;        // always 0
    uint8_t  flags;       // type and attributes (e.g., 0x8E for 32-bit interrupt gate, ring 0)
    uint16_t base_high;   // upper 16 bits of ISR address
} __attribute__((packed));

struct idt_ptr {
    uint16_t limit;       // size of IDT - 1
    uint32_t base;        // address of first idt_entry
} __attribute__((packed));

#define IDT_ENTRIES 256

struct idt {
    struct idt_entry idt[IDT_ENTRIES];
    struct idt_ptr   idtp;
    uint8_t master_enable;
    uint8_t slave_enable;
};

// https://f.osdev.org/viewtopic.php?t=16864
#define PIC_REMAP_MASTER 0x20
#define PIC_REMAP_SLAVE  0x28


void idt_set_entry(struct idt *idt,
                   uint8_t num,        // interrupt vector
                   uint32_t base,      // isr handler code address
                   uint16_t selector,  // code segment
                   uint8_t flags) {
    idt->idt[num].base_low  = base & 0xFFFF;
    idt->idt[num].base_high = (base >> 16) & 0xFFFF;
    idt->idt[num].selector  = selector;
    idt->idt[num].zero      = 0;
    idt->idt[num].flags     = flags;
}

static inline void lidt(struct idt_ptr *ptr) {
    __asm__ volatile("lidt (%0)" : : "r"(ptr) : "memory");
}



// https://claude.ai/chat/c083247d-58de-4198-88e8-d762f99823a3

// The 8259 PIC maps IRQ 0–7 to interrupts 0x08–0x0F by default,
// which collides with CPU exceptions in protected mode (e.g., IRQ
// 0 = timer hits the same vector as Double Fault). They need to be
// remapped.

// https://claude.ai/chat/aa3d7243-6be1-4dff-8767-94d3fa007971
// ICWx init sequence.

static inline void init_pic(void) {

    // Remap PIC1 (master) to 0x20-0x27, PIC2 (slave) to 0x28-0x2F
    // ICW1: init + ICW4 needed, both master PIC (0x20) and slave PIC (0xA0)
    outb(0x20, 0x11);
    outb(0xA0, 0x11);
    // ICW2: master starts at 0x20, slave at 0x28
    outb(0x21, PIC_REMAP_MASTER);
    outb(0xA1, PIC_REMAP_SLAVE);
    // ICW3: master has slave on IRQ2, slave cascade identity
    outb(0x21, 0x04);
    outb(0xA1, 0x02);
    // ICW4: 8086 mode
    outb(0x21, 0x01);
    outb(0xA1, 0x01);

    // After the 4 ICWx are written, the x1 ports take the mask bits.
    // 1=masked, 0=active.

    // Mask all IRQs and turn on interrupts.
    outb(0x21, 0b11111111);
    outb(0xA1, 0b11111111);

    sti();
}

static inline void __attribute__((__always_inline__)) isr_begin(void) {
    __asm__ volatile(
        "pusha"           "\n\t"
        "cld"             "\n\t"   // set movs direction
        );
}
static inline void __attribute__((__always_inline__)) isr_end(void) {
    __asm__ volatile(
        "popa"            "\n\t"
        "iret"            "\n\t"
        );

}

struct idt_isr {
    void (*keyboard_isr)(void);
    void (*com1_isr)(void);
};

void idt_set_master(struct idt *idt,
                    int irq,
                    void (*isr)(void)) {
    // 0x08 = kernel code segment
    // 0x8E = present | ring 0 | 32-bit interrupt gate
    idt_set_entry(idt, PIC_REMAP_MASTER + irq, (uint32_t)isr, 0x08, 0x8E);
    idt->master_enable |= (1 << irq);
}

static inline void idt_init(struct idt *idt,
                            const struct idt_isr *isr) {

    init_pic();

    idt->idtp.limit = (sizeof(struct idt_entry) * IDT_ENTRIES) - 1;
    idt->idtp.base  = (uint32_t)&idt->idt;

    // Zero out all entries
    for (int i = 0; i < IDT_ENTRIES; i++) {
        idt_set_entry(idt, i, 0, 0, 0);
    }
    idt->master_enable = 0;
    idt->slave_enable = 0;
    if (isr) {
        if (isr->keyboard_isr) {
            idt_set_master(idt, 1, isr->keyboard_isr);
        }
        if (isr->com1_isr) {
            idt_set_master(idt, 4, isr->com1_isr);
            com1_init();
            com1_putstr("kernel\r\n");
        }
    }

    outb(0x21, ~idt->master_enable);
    outb(0xA1, ~idt->slave_enable);

    lidt(&idt->idtp);
}

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
static inline void set_cursor_pos(uint16_t pos) {
    crtc_write(CRTC_CURSOR_LOC_LO, pos & 0xFF);
    crtc_write(CRTC_CURSOR_LOC_HI, pos >> 8);
}




#endif
