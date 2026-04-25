#ifndef HW_I686_H
#define HW_I686_H

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

    // Mask all IRQs and turn on interrupts.
    outb(0x21, 0b11111111);
    outb(0xA1, 0b11111111);

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
            com1_init();
            com1_putstr("kernel\r\n");
        }
        if (isr->rtl8139.irq) {
            LOG("enable rtl8139 irq %d\n", isr->rtl8139.irq);
            idt_set(idt, isr->rtl8139.irq, isr->rtl8139.isr);
        }
    }

    LOG("irq enables %02x %02x\n",
        idt->master_enable,
        idt->slave_enable);

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



// Read a 32-bit word from PCI config space
#define PCI_CONFIG_ADDRESS  0xCF8
#define PCI_CONFIG_DATA     0xCFC

static inline uint32_t pci_config_addr(uint8_t bus, uint8_t dev, uint8_t func, uint8_t offset) {
    uint32_t addr = (1u << 31)           // Enable bit
                  | ((uint32_t)bus  << 16)
                  | ((uint32_t)dev  << 11)
                  | ((uint32_t)func <<  8)
                  | (offset & 0xFC);     // DWORD-aligned
    return addr;
}
static inline uint32_t pci_config_read32(uint8_t bus, uint8_t dev, uint8_t func, uint8_t offset) {
    uint32_t addr = pci_config_addr(bus, dev, func, offset);
    outl(PCI_CONFIG_ADDRESS, addr);
    return inl(PCI_CONFIG_DATA);
}

static inline void pci_config_write32(uint8_t bus, uint8_t dev, uint8_t func, uint8_t offset,
                                      uint32_t value) {
    uint32_t addr = pci_config_addr(bus, dev, func, offset);
    outl(PCI_CONFIG_ADDRESS, addr);
    outl(PCI_CONFIG_DATA, value);
}
static inline void pci_config_write16(uint8_t bus, uint8_t dev, uint8_t func, uint8_t offset,
                                      uint16_t value) {
    uint32_t addr = pci_config_addr(bus, dev, func, offset);
    outw(PCI_CONFIG_ADDRESS, addr);
    outw(PCI_CONFIG_DATA, value);
}



static inline uint16_t pci_config_read16(uint8_t bus, uint8_t dev, uint8_t func, uint8_t off) {
    uint32_t d = pci_config_read32(bus, dev, func, off & 0xFC);
    return (uint16_t)(d >> ((off & 2) * 8));
}
static inline uint8_t pci_config_read8(uint8_t bus, uint8_t dev, uint8_t func, uint8_t off) {
    uint32_t d = pci_config_read32(bus, dev, func, off & 0xFC);
    return (uint8_t)(d >> ((off & 3) * 8));
}
struct pci_function {
    uint8_t  bus;
    uint8_t  dev;
    uint8_t  func;
    uint16_t vendor;
    uint16_t device;
    uint8_t  class;
    uint8_t  subclass;
    uint8_t  hdr;
};
struct pci_cb {
    void (*fun)(void *ctx, struct pci_function *);
    void *ctx;
};
static inline void check_function(struct pci_cb *cb,
                                  uint8_t bus, uint8_t dev, uint8_t func) {
    struct pci_function pci_function = {
        .bus      = bus,
        .dev      = dev,
        .func     = func,
        .vendor   = pci_config_read16(bus, dev, func, 0x00),
        .device   = pci_config_read16(bus, dev, func, 0x02),
        .class    = pci_config_read8 (bus, dev, func, 0x0B),
        .subclass = pci_config_read8 (bus, dev, func, 0x0A),
        .hdr      = pci_config_read8 (bus, dev, func, 0x0E) & 0x7F,
    };
    // ... record device, inspect BARs, etc.
    cb->fun(cb->ctx, &pci_function);

#if 0
    // Recurse into PCI-to-PCI bridges (header type 01h)
    if (hdr == 0x01) {
        uint8_t secondary = pci_config_read8(bus, dev, func, 0x19);
        // secondary bus already enumerated by the 0..255 sweep above,
        // but if you do recursive enumeration instead, scan it here.
    }
#endif
}
static void check_device(struct pci_cb *cb, uint8_t bus, uint8_t dev) {
    uint16_t vendor = pci_config_read16(bus, dev, 0, 0x00);
    if (vendor == 0xFFFF) return;   // no device present

    check_function(cb, bus, dev, 0);

    // Is it multi-function? Header type bit 7 says so.
    uint8_t hdr = pci_config_read8(bus, dev, 0, 0x0E);
    if (hdr & 0x80) {
        for (uint8_t func = 1; func < 8; func++) {
            if (pci_config_read16(bus, dev, func, 0x00) != 0xFFFF) {
                check_function(cb, bus, dev, func);
            }
        }
    }
}


static inline void pci_enumerate(struct pci_cb *cb) {
    uint16_t nb_busses = 256;
    for (uint16_t bus = 0; bus < nb_busses; bus++) {
        for (uint8_t dev = 0; dev < 32; dev++) {
            //LOG("%02x %02x\n", bus, dev);
            check_device(cb, bus, dev);
        }
    }
}


/* RTL8139 */

#define RTL_IDR0        0x00   // MAC address (6 bytes)
#define RTL_MAR0        0x08   // Multicast filter
#define RTL_RBSTART     0x30   // RX buffer start address
#define RTL_CMD         0x37   // Command register
#define RTL_CAPR        0x38   // Current address of packet read
#define RTL_CBR         0x3A
#define RTL_IMR         0x3C   // Interrupt mask
#define RTL_ISR         0x3E   // Interrupt status
#define RTL_RCR         0x44   // RX config
#define RTL_MPC         0x4C   // Missed Packet Counter
#define RTL_9346CR      0x50   // EEPROM Command Register
#define RTL_CONFIG1     0x52   // Config 1
#define RTL_MSR         0x58   // Media Status Register

// CMD bits
#define CMD_RST         0x10
#define CMD_RE          0x08   // RX enable
#define CMD_TE          0x04   // TX enable

// ISR/IMR bits
#define INT_ROK         (1<<0) // RX OK
#define INT_RER         (1<<1) // RX error
#define INT_TOK         (1<<2) // TX OK
#define INT_TER         (1<<3) // TX error
#define INT_RXOVW       (1<<4) // RX buffer overflow
#define INT_PUN         (1<<5) // Packet Underrun or Link Change
#define INT_FOVW        (1<<6) // RX fifo overflow

// RCR bits
#define RCR_AAP         (1<<0) // Accept all packets (promiscuous)
#define RCR_APM         (1<<1) // Accept physical match
#define RCR_AM          (1<<2) // Accept multicast
#define RCR_AB          (1<<3) // Accept broadcast
#define RCR_WRAP        (1<<7) // Ring wrap enable — keep this SET
#define RCR_MXDMA_UNLIM (7<<8) // Max DMA burst size unlimited

#define RCR_RXFTH_NONE  (7<<13) // No receive threshold


// RCR bits 12:11 — RBLEN
//  00  →  8K  + 16 bytes
//  01  →  16K + 16 bytes
//  10  →  32K + 16 bytes
//  11  →  64K + 16 bytes

// 0 = 8k
// 1 = 16k
// 2 = 32k
// 3 = 64k

// There seems to be no reason to pick anything less than the maximum
// 0 is not enough in practice
#define RCR_RBLEN_TAG 0
#define RTL8139_RX_BUF_LEN (1<<(RCR_RBLEN_TAG+13))
#define RCR_RBLEN (RCR_RBLEN_TAG<<11)

struct rtl8139 {
    uint8_t  rx_buf[RTL8139_RX_BUF_LEN + 2048];
    uint32_t iobase;
    uint8_t  irq;
    uint32_t rx_offset;
    uint8_t  mac[6];
};

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

static inline void rtl8139_rx_poll(struct rtl8139 *s) {
    // CMD bit 0 = RX buffer empty
    uint32_t mask = (RTL8139_RX_BUF_LEN - 1) & (~3);
    while (!(inb(s->iobase + RTL_CMD) & 0x01)) {
        uint8_t *entry = s->rx_buf + s->rx_offset;
        uint32_t o0 = s->rx_offset;

        uint16_t rx_status = *(uint16_t *)(entry + 0);
        uint16_t rx_len    = *(uint16_t *)(entry + 2); // includes 4-byte CRC

        if (!(rx_status & 0x0001)) {
            // ROK not set — bad packet or not yet ready
            break;
        }

        uint8_t  *pkt     = entry + 4;
        uint16_t  pkt_len = rx_len - 4; // strip CRC

        // Hand packet up to your network stack
        // ethernet_receive(pkt, pkt_len);
        //LOG("pkt len=%d\n", pkt_len);
        //log_hex(pkt, pkt_len);

        // Advance read pointer — DWORD aligned, wrapped.
        s->rx_offset = (s->rx_offset + 4 + rx_len + 3) & mask;
        uint32_t o1 = s->rx_offset;

        //LOG("offset %04x->%04x\n", o0, o1);
        volatile uint8_t *top_right = (void*)(0xB8000 + 2*79);
        (*top_right)++;;

        

        // Tell card we've consumed up to here.
        // CAPR is written as rx_offset-16, a hardware quirk
        outw(s->iobase + RTL_CAPR, (s->rx_offset - 16) & mask);
    }
}

static inline void halt(void) {
  again:
    goto again;
}

static inline void rtl8139_reset(struct rtl8139 *s) {
    // Enable RX + TX
    outb(s->iobase + RTL_CMD, CMD_RE | CMD_TE);

    // Set RX config:
    //    - Accept broadcast + physical match + multicast
    //    - 16K ring, wrap enabled, unlimited DMA
    uint32_t rcr_val =
        RCR_AAP | // all packets
        RCR_AB | RCR_APM | RCR_AM |
        RCR_WRAP | RCR_RBLEN | RCR_MXDMA_UNLIM |
        RCR_RXFTH_NONE;

    outl(s->iobase + RTL_RCR, rcr_val);

    s->rx_offset = 0;

}


static inline void rtl8139_isr_inner(struct rtl8139 *s) {
    uint16_t status = inw(s->iobase + RTL_ISR);
    // Acknowledge all raised interrupts
    outw(s->iobase + RTL_ISR, status);

    uint16_t status_ = inw(s->iobase + RTL_ISR);

    //LOG("rtl8139 interrupt %02x %02x\n", status, status_);


    if (status & INT_ROK) {
        rtl8139_rx_poll(s);
    }
    if (status & (INT_RER | INT_RXOVW)) {
        LOG("rtl8139 error %04x\n", status);
        // Disable receiver
        outb(s->iobase + RTL_CMD, CMD_TE);
        // Re-initialize
        rtl8139_reset(s);
    }


    // Caller will EOI to PIC
}

static inline uint32_t pci_function_read32(const struct pci_function *f, uint8_t offset) {
    return pci_config_read32(f->bus, f->dev, f->func, offset);
}
static inline uint16_t pci_function_read16(const struct pci_function *f, uint8_t offset) {
    return pci_config_read16(f->bus, f->dev, f->func, offset);
}
static inline void pci_function_write16(const struct pci_function *f, uint8_t offset, uint16_t val) {
    return pci_config_write16(f->bus, f->dev, f->func, offset, val);
}



static inline void rtl8139_init(struct rtl8139 *s,
                                const struct pci_function *f) {
    s->iobase = pci_function_read32(f, 0x10) & ~3; // BAR0, strip I/O bit
    s->irq    = pci_function_read32(f, 0x3C) & 0xFF;


    for (int i = 0; i < 6; i++) {
        s->mac[i] = inb(s->iobase + RTL_IDR0 + i);
    }

    LOG("RTL8139_RX_BUF_LEN=%d, RCR_RBLEN_TAG=%d\n",
        RTL8139_RX_BUF_LEN, RCR_RBLEN_TAG);


    // Enable PCI Bus Mastering + I/O Space
    uint16_t cmd = pci_function_read16(f, 0x04);
    pci_function_write16(f, 0x04, cmd | 0x5);
    //uint16_t cmd_ = pci_function_read16(f, 0x04);
    // LOG("cmd=%04x->cmd=%04x\n", cmd, cmd_);

    /* Unlock config registers (some real hardware needs this) */
    // outb(s->iobase + 0x50, 0xC0); /* 9346CR: config write enable */

    // Power on
    outb(s->iobase + RTL_CONFIG1, 0x00);

    // Software reset — wait for RST bit to clear
    outb(s->iobase + RTL_CMD, CMD_RST);
    while (inb(s->iobase + RTL_CMD) & CMD_RST);

    // Give card the physical RX buffer address
    outl(s->iobase + RTL_RBSTART, (uint32_t)s->rx_buf);

    // Also called from isr
    rtl8139_reset(s);

    // Accept all multicast
    outl(s->iobase + RTL_MAR0,   0xFFFFFFFF);
    outl(s->iobase + RTL_MAR0+4, 0xFFFFFFFF);

    // Unmask RX OK + RX error + overflow interrupts
    outw(s->iobase + RTL_IMR, INT_ROK | INT_RER | INT_RXOVW);

    /* Lock config registers */
    //outb(s->iobase + 0x50, 0x00);          /* 9346CR: normal mode */


    // uint8_t msr = inb(s->iobase + 0x58);
    // LOG("msr = %02x\n", msr);
}

#endif
