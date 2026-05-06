#ifndef HW_I686_PCI_H
#define HW_I686_PCI_H

#include "hw_i686_io.h"



// Read a 32-bit word from PCI config space
#define PCI_CONFIG_ADDRESS  0xCF8
#define PCI_CONFIG_DATA     0xCFC

#define PCI_CFG_IRQ  0x3C
#define PCI_CFG_BAR0 0x10
#define PCI_CFG_BAR1 0x14


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


static inline uint32_t pci_function_read32(const struct pci_function *f, uint8_t offset) {
    return pci_config_read32(f->bus, f->dev, f->func, offset);
}
static inline uint16_t pci_function_read16(const struct pci_function *f, uint8_t offset) {
    return pci_config_read16(f->bus, f->dev, f->func, offset);
}
static inline void pci_function_write16(const struct pci_function *f, uint8_t offset, uint16_t val) {
    return pci_config_write16(f->bus, f->dev, f->func, offset, val);
}



struct pci_cb {
    void (*fun)(void *ctx, struct pci_function *);
    void *ctx;
};
static inline void pci_check_function(struct pci_cb *cb,
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


static void pci_check_device(struct pci_cb *cb, uint8_t bus, uint8_t dev) {
    uint16_t vendor = pci_config_read16(bus, dev, 0, 0x00);
    if (vendor == 0xFFFF) return;   // no device present

    pci_check_function(cb, bus, dev, 0);

    // Is it multi-function? Header type bit 7 says so.
    uint8_t hdr = pci_config_read8(bus, dev, 0, 0x0E);
    if (hdr & 0x80) {
        for (uint8_t func = 1; func < 8; func++) {
            if (pci_config_read16(bus, dev, func, 0x00) != 0xFFFF) {
                pci_check_function(cb, bus, dev, func);
            }
        }
    }
}

static inline void pci_enumerate(struct pci_cb *cb) {
    uint16_t nb_busses = 256;
    for (uint16_t bus = 0; bus < nb_busses; bus++) {
        for (uint8_t dev = 0; dev < 32; dev++) {
            //LOG("%02x %02x\n", bus, dev);
            pci_check_device(cb, bus, dev);
        }
    }
}



#endif
