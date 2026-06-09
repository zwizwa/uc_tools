#ifndef HW_I686_MCS9865
#define HW_I686_MCS9865

#include "hw_i686_com.h"
#include "hw_i686_par.h"
#include "hw_i686_pci.h"

#define mcs9865_vendor   0x9710
#define mcs9865_device   0x9865

#define MCS9865_BAR0 0x10

// UART0 enumerates but is not populated on one board, so just pick
// the second one as the main one.
#define MCS9865_MAIN_UART 1

struct mcs9865_uart {
    struct uart uart; // 16550 compatible registers, irq
    uint16_t ioextra; // clock/prescaler registers
};
struct mcs9865 {
    struct mcs9865_uart uart[2];
    struct par par;
};

/* I have two cards.  One has 2 UARTS + 1 PAR, one has 1 UART (1
   footprint not populated) and 1 PAR, but the UART still shows up as
   a function.  The datasheet calls it a "PCI to Dual Serial and Dual
   Parallel Controller". */

static inline void mcs9865_init_uart(struct mcs9865 *s,
                                     struct mcs9865_uart *u,
                                     const struct pci_function *f) {
    u->uart.irq     = pci_function_read32(f, PCI_CFG_IRQ) & 0xFF;
    u->uart.iobase  = pci_function_read32(f, PCI_CFG_BAR0) & ~3;
    u->ioextra      = pci_function_read32(f, PCI_CFG_BAR1) & ~3;

    LOG("mcs9865 uart f=%d io=%04x,%04x irq=%d\n",
        f->func,
        u->uart.iobase,
        u->ioextra,
        u->uart.irq);

    uart_init(&u->uart, 1);

    //uart_putstr(&s->uart, "mcs9865 ok\n");

}

static inline void mcs9865_init_par(struct mcs9865 *s,
                                    struct par *p,
                                    const struct pci_function *f) {
    p->spp = pci_function_read32(f, PCI_CFG_BAR0) & ~3;
    p->epp = pci_function_read32(f, PCI_CFG_BAR0) & ~3;

    LOG("mcs9865 par f=%d spp=%04x epp=%04x\n",
        f->func,
        p->spp,
        p->epp);

    // I/O Space Enable bit
    uint16_t cmd = pci_function_read16(f, 0x04);
    LOG("mcs9865 par 04=%04x\n", cmd);
    pci_function_write16(f, 0x04, cmd | 0x0001);

    par_pulse(p);
}


static inline void mcs9865_init(struct mcs9865 *s, const struct pci_function *f) {

    if (f->func == 0) { // Only print this once
        uint32_t sub_vendor = pci_function_read16(f, PCI_CFG_SUBSYSTEM_VENDOR_ID);
        uint32_t sub_id     = pci_function_read16(f, PCI_CFG_SUBSYSTEM_ID);
        LOG("mcs9865 subsystem id %04x:%04x\n", sub_vendor, sub_id);
    }


    /* Only initialize funcion 0, the first uart. */
    switch (f->func) {
    case 0:
        mcs9865_init_uart(s, &s->uart[0], f);
        break;
    case 1:
        mcs9865_init_uart(s, &s->uart[1], f);
        break;
    case 2:
        mcs9865_init_par(s, &s->par, f);
        break;
    }

}

#endif
