#ifndef HW_I686_ICE_H
#define HW_I686_ICE_H

#define ice_vendor  0x1412  /* ICE Emsemble / VIA Technologies */
#define ice_device  0x1712  /* ICE1712 / Envy24 */

struct ice {
    // bar0-3
    uint32_t ccs;      // Envy24 Chip Control
    uint32_t ddma;     // Legacy SB-compatible DMA
    uint32_t dmapath;  // DirectSound DMA
    uint32_t profi;    // Professional multi-track

    uint8_t irq;
};

#define ICE_CONTROL          0x00   /* byte */
#define ICE_CTRL_RESET       0x80
#define ICE_CTRL_NATIVE      0x01


static inline void ice_init(struct ice *s,
                             const struct pci_function *f) {

    //s->mmio = (uint8_t*)(pci_function_read32(f, PCI_CFG_BAR1) & ~0xF);
    //s->irq  = pci_function_read32(f, PCI_CFG_IRQ) & 0xFF;
    //LOG("dp83815 mmio=%08x irq=%d\n", s->mmio, s->irq);
    uint32_t sub_vendor = pci_function_read16(f, PCI_CFG_SUBSYSTEM_VENDOR_ID);
    uint32_t sub_id     = pci_function_read16(f, PCI_CFG_SUBSYSTEM_ID);
    LOG("ice sub %04x:%04x\n", sub_vendor, sub_id); // ASSERT sub_id == 0xd630
    if (sub_id != 0xd630) return;

    s->ccs     = pci_function_bar0_io(f);
    s->ddma    = pci_function_bar1_io(f);
    s->dmapath = pci_function_bar2_io(f);
    s->profi   = pci_function_bar3_io(f);
    s->irq     = pci_function_irq(f);
    LOG("ice ccs=%4x, profi=%4x, irq=%d\n", s->ccs, s->profi, s->irq);

#if 0
    /* Soft-reset before touching EEPROM -- the I2C controller is part of
       the same block and may be in an undefined state at power-on. */
    outb(e->port + ICE_CONTROL, ICE_CTRL_RESET | ICE_CTRL_NATIVE);
    udelay(200);
    outb(e->port + ICE_CONTROL, ICE_CTRL_NATIVE);
    udelay(200);
#endif


}


#endif
