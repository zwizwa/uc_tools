#ifndef HW_I686_MCS9865
#define HW_I686_MCS9865

#include "hw_i686_com.h"
#include "hw_i686_pci.h"

#define MCS9865_VENDOR   0x9710
#define MCS9865_DEVICE   0x9865

#define MCS9865_BAR0 0x10



struct mcs9865 {
    struct uart uart; // 16550 compatible registers, irq
    uint16_t ioextra; // clock/prescaler registers
};

static inline void mcs9865_init(struct mcs9865 *s, const struct pci_function *f) {

    /* Only initialize funcion 0, the first uart. */
    if (f->func != 0) {
        LOG("mcs9865 ignore function %d\n", f->func);
        return;
    }

    s->uart.irq     = pci_function_read32(f, PCI_CFG_IRQ) & 0xFF;
    s->uart.iobase  = pci_function_read32(f, PCI_CFG_BAR0) & ~3;
    s->ioextra      = pci_function_read32(f, PCI_CFG_BAR1) & ~3;
    LOG("mcs9865 io=%04x,%04x irq=%d\n",
        s->uart.iobase,
        s->ioextra,
        s->uart.irq);
    LOG("uart init io=%04x\n", s->uart.iobase);
    uart_init(&s->uart);
    uart_putstr(&s->uart, "mcs9865 ok\n");

}

#endif
