#ifndef HW_I686_MCS9904
#define HW_I686_MCS9904

#include "hw_i686_com.h"
#include "hw_i686_pci.h"

#define mcs9904_vendor   0x9710
#define mcs9904_device   0x9904

#define MCS9904_BAR0 0x10



struct mcs9904 {
    struct uart uart; // 16550 compatible registers, irq
    uint16_t ioextra; // clock/prescaler registers
};

static inline void mcs9904_init(struct mcs9904 *s, const struct pci_function *f) {

    s->uart.irq     = pci_function_read32(f, PCI_CFG_IRQ) & 0xFF;
    s->uart.iobase  = pci_function_read32(f, PCI_CFG_BAR0) & ~3;
    s->ioextra      = pci_function_read32(f, PCI_CFG_BAR1) & ~3;
    LOG("mcs9904 io=%04x,%04x irq=%d\n",
        s->uart.iobase,
        s->ioextra,
        s->uart.irq);
    LOG("uart init io=%04x\n", s->uart.iobase);
    uart_init(&s->uart, 1);
    //uart_putstr(&s->uart, "mcs9904 ok\n");

}

#endif
