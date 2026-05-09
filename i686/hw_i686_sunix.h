#ifndef HW_I686_SUNIX_H
#define HW_I686_SUNIX_H

#include "hw_i686_com.h"
#include "hw_i686_pci.h"

#define SUNIX_VENDOR 0x1fd4
#define SUNIX_DEVICE 0x1999

struct sunix {
    struct uart uart;
};

static inline void sunix_init(struct sunix *s,
                              const struct pci_function *f) {
    s->uart.iobase = pci_function_read32(f, PCI_CFG_BAR0) & ~3;
    s->uart.irq    = pci_function_read32(f, PCI_CFG_IRQ) & 0xFF;

    uart_init(&s->uart, 8);
    LOG("sunix io=0x%x irq=%d\n", s->uart.iobase, s->uart.irq);
    uart_putstr(&s->uart, "sunix init\n");

}

#endif
