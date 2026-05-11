#ifndef HW_I686_ECHO_H
#define HW_I686_ECHO_H

#define ECHO_VENDOR  0x1057  /* Motorola */
#define ECHO_DEVICE  0x1801  /* Echo */

struct echo {
    uint32_t mmio;
    uint8_t irq;
};

static inline void echo_init(struct echo *s,
                             const struct pci_function *f) {
    //s->mmio = (uint8_t*)(pci_function_read32(f, PCI_CFG_BAR1) & ~0xF);
    //s->irq  = pci_function_read32(f, PCI_CFG_IRQ) & 0xFF;
    //LOG("dp83815 mmio=%08x irq=%d\n", s->mmio, s->irq);
    uint32_t sub_vendor = pci_function_read16(f, PCI_CFG_SUBSYSTEM_VENDOR_ID);
    uint32_t sub_id     = pci_function_read16(f, PCI_CFG_SUBSYSTEM_ID);
    LOG("echo %04x:%04x\n", sub_vendor, sub_id);
    
}


#endif
