#ifndef HW_I686_EMU10K_H
#define HW_I686_EMU10K_H

#define emu10k_vendor  0x1102
#define emu10k_device  0x0002

struct emu10k {
    uint16_t iobase;
    uint8_t irq;
};

static inline void emu10k_init(struct emu10k *s,
                               const struct pci_function *f) {
    s->iobase = pci_function_bar0_io(f);
    s->irq    = pci_function_irq(f);
    LOG("emu10k io=%04x irq=%d\n", s->iobase, s->irq);

    // This triggers sometimes.  Related to plugging in the scope?  Ground issue?
    ASSERT_WARN(pci_function_read16(f, 0x04) & (1<<2)); // Bus master enabled.

    // TODO:
    // 1. switch on amp, verify noise level
    // 2. turn on AC97
}


#endif
