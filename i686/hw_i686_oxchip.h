#ifndef HW_I686_OXCHIP_H
#define HW_I686_OXCHIP_H

/* Still exploring.
   It seems to be in 950 mode, two memory bars */

#include "hw_i686_com.h"
#include "hw_i686_pci.h"

#define oxchip_vendor 0x1415
#define oxchip_device 0xc158

/* OXPCIe952 data sheet: the Device ID contains encoding:
   1100 0001 0xxx x x xx
              ||| | | ||
             mode | | ||
            uart_en | ||
              gpio_en ||
             function_nb

   This is 2 native UARTs
   BAR0 MEM 16K UART & UDMA
   BAR1 MEM  2M All visible modules & MSI-X (Used for MSI-X)
   BAR2 MEM  2M All visible modules & MSI-X (Used for EEPROM)  -> zero on this card

   Table 14 has register mapping.

   0x0000         Class code & Rev-ID
   0x0004         Decimal Number of UARTs
   0x0008         Global UART IRQ Status
   0x000C         Global UART IRQ Enable
   0x0010         Global UART IRQ Disable
   0x0014         Global UART Wake Enable
   0x0018         Global UART Wake Disable
   0x001C..0x0FFF Reserved
   0x1000..0x10FF UART[0] registers(1)
   0x1100..0x110F UART[0] DMA channels(2)
   0x2000..0x10FF UART[1] registers(1)
   0x2100..0x110F UART[1] DMA channels(2)
*/

struct oxchip {
    uint32_t bar0, bar1;
    uint8_t irq;
};

static inline void oxchip_init(struct oxchip *s,
                              const struct pci_function *f) {
    s->bar0 = pci_function_bar0_mem(f);
    s->bar1 = pci_function_bar1_mem(f);
    s->irq  = pci_function_irq(f);

    LOG("oxchip mmio0=%08x mmio1=%08x irq=%d\n",
        s->bar0, s->bar1, s->irq);

    // I don't trust Claude much here.  Maybe first figure out what I
    // want to do here.  E.g. create a DMX node?  Run Nano code?  I
    // think I first want a high-precision timer, see if I can
    // generate breaks etc.

    // https://claude.ai/chat/14268e91-b36b-4aea-8c51-a4db6a782c09

}

#endif
