#ifndef HW_I686_OX16PCI952_H
#define HW_I686_OX16PCI952_H

/* This is a serial + parallel card. */

#include "hw_i686_com.h"
#include "hw_i686_pci.h"

#define ox16pci952_vendor 0x1415
#define ox16pci952_device 0x9521

struct ox16pci952 {
    uint16_t bar0, bar1;
    struct uart uart; // 16550 compatible registers, irq
};

static inline void ox16pci952_init(struct ox16pci952 *s,
                              const struct pci_function *f) {
    s->uart.iobase = pci_function_bar0_io(f);
    s->bar1        = pci_function_bar1_io(f);
    s->uart.irq    = pci_function_irq(f);

    LOG("ox16pci952 io0=%04x io1=%04x irq=%d\n",
        s->uart.iobase, s->bar1, s->uart.irq);

    uart_init(&s->uart, 1);
    // uart_putstr(&s->uart, "ox16pci952\n");


}

#endif
