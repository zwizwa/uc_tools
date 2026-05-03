#ifndef HW_I686_INTERRUPTS
#define HW_I686_INTERRUPTS

#include "hw_i686_io.h"

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
                   uint8_t  num,       // interrupt vector
                   uint32_t base,      // isr handler code address
                   uint16_t selector,  // code segment
                   uint8_t  flags) {
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

    // Mask all IRQs
    outb(0x21, 0b11111111);
    outb(0xA1, 0b11111111);

    // Send EOI for anything that might be in-service.  This as added
    // to make "restart" command to work when entered on the keyboard,
    // likely has to do with the enter key release interrupt that
    // needs to be acknowledged.
    outb(0x20, 0x20);
    outb(0xA0, 0x20);



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
    struct {
        void (*isr)(void);
        uint8_t irq;
    } rtl8139;
};

void idt_set_master(struct idt *idt,
                    int irq,
                    void (*isr)(void)) {
    // 0x08 = kernel code segment
    // 0x8E = present | ring 0 | 32-bit interrupt gate
    idt_set_entry(idt, PIC_REMAP_MASTER + irq, (uint32_t)isr, 0x08, 0x8E);
    idt->master_enable |= (1 << irq);
}
void idt_set_slave(struct idt *idt,
                   int irq,
                   void (*isr)(void)) {
    // 0x08 = kernel code segment
    // 0x8E = present | ring 0 | 32-bit interrupt gate
    idt_set_entry(idt, PIC_REMAP_SLAVE + irq - 8, (uint32_t)isr, 0x08, 0x8E);
    idt->slave_enable |= (1 << (irq - 8));
    idt->master_enable |= (1 << 2); // cascade
}


void idt_set(struct idt *idt,
             int irq,
             void (*isr)(void)) {
    if (irq < 8) {
        idt_set_master(idt, irq, isr);
    }
    else {
        idt_set_slave(idt, irq, isr);
    }
}



// Note that this leaves interrupts off such that caller can do some
// more setup before enabling interrupts.
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
            idt_set(idt, 1, isr->keyboard_isr);
        }
        if (isr->com1_isr) {
            idt_set(idt, 4, isr->com1_isr);
        }
        if (isr->rtl8139.irq) {
            //LOG("enable rtl8139 irq %d\n", isr->rtl8139.irq);
            idt_set(idt, isr->rtl8139.irq, isr->rtl8139.isr);
        }
    }

    //LOG("irq enables %02x %02x\n",
    //    idt->master_enable,
    //    idt->slave_enable);

    outb(0x21, ~idt->master_enable);
    outb(0xA1, ~idt->slave_enable);


    lidt(&idt->idtp);
}


#endif
