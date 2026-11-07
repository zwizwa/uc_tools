#ifndef HW_I686_OXPCIE952_H
#define HW_I686_OXPCIE952_H

/* Two native 950 UARTs, MMIO.
   Card: Antaira MSC-202B1(-SI), RS422/485, DB25 to 2x DB9M octopus.
   - DB9M: 1 DATA- (TXD-), 2 DATA+ (TXD+), 3 RXD+, 4 RXD-, 5 GND
   - jumpers per port: mode 485-2W/4W/422, 120R term, echo, TXD AUT/MAN
   - AUT (default): RS485 direction in hardware (ATTA), DTR not used
   Docs:
   - quick start, pinout + jumpers:
     https://westwardsales.com/product/product/download?product_id=3610&download_id=962
   - data sheet:
     https://westwardsales.com/product/product/download?product_id=3610&download_id=961
   - OX16C950 core (ICR, TCR):
     https://manualmachine.com/oxford/ox16c950/8444714-technical-data
   - Linux drivers/tty/serial/8250/8250_pci.c, pci_oxsemi_tornado_*

   Antairia pinout, RS485 mode:
   1 DMX- blue
   2 DMX+ yellow
   5 GND  black

*/

#include "hw_i686_com.h"
#include "hw_i686_pci.h"

#define oxpcie952_vendor 0x1415
#define oxpcie952_device 0xc158

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

   Linux 8250_pci puts UART[1] at 0x1200, not 0x2000.
*/

/* 950 core, see OX16C950 data sheet
   - ICR registers: index in SPR, data in ICR (LSR address)
   - 62.5 MHz clock, prescaler off: baud = clk / (tcr * div) */
#define OX950_EFR  2  // with LCR=0xBF
#define OX950_ICR  5
#define OX950_SPR  7
#define OX950_TCR  0x02
#define OX950_CSR  0x0C
#define OX950_CLK  62500000

#define OXPCIE952_NB_UART 2

struct oxpcie952 {
    uint32_t bar0, bar1;
    uint8_t irq;
    struct uart uart[OXPCIE952_NB_UART];
    uint8_t rx[256];  // both uarts, for testing
    uint8_t rx_head;
};
static void oxpcie952_rx(void *ctx, uint8_t byte) {
    struct oxpcie952 *s = ctx;
    s->rx[s->rx_head++] = byte;
}
static inline void oxpcie952_isr_inner(struct oxpcie952 *s) {
    for (int i = 0; i < OXPCIE952_NB_UART; i++) {
        uart_isr(&s->uart[i], oxpcie952_rx, s);
    }
}

static inline void ox950_icr_write(struct uart *u, uint8_t idx, uint8_t val) {
    uart_out(u, OX950_SPR, idx);
    uart_out(u, OX950_ICR, val);
}
static inline void ox950_init(struct uart *u, uint32_t baud) {
    uint32_t best_tcr = 16, best_div = 1, best_err = ~0;
    for (uint32_t tcr = 16; tcr >= 4; tcr--) {
        uint32_t div = (OX950_CLK / tcr + baud / 2) / baud;
        if (!div || div > 0xFFFF) continue;
        uint32_t b = OX950_CLK / (tcr * div);
        uint32_t err = b > baud ? b - baud : baud - b;
        if (err < best_err) { best_err = err; best_tcr = tcr; best_div = div; }
    }
    ox950_icr_write(u, OX950_CSR, 0);  // reset
    uart_out(u, UART_LCR, 0xBF);
    uart_out(u, OX950_EFR, 0x10);      // enhanced mode
    uart_out(u, UART_LCR, 0);
    ox950_icr_write(u, OX950_TCR, best_tcr & 15);
    uart_init(u, best_div);
    LOG("ox950 %08x baud=%d tcr=%d div=%d\n",
        (uint32_t)u->mmio, OX950_CLK / (best_tcr * best_div),
        best_tcr, best_div);
}

static inline void oxpcie952_init(struct oxpcie952 *s,
                              const struct pci_function *f) {
    s->bar0 = pci_function_bar0_mem(f);
    s->bar1 = pci_function_bar1_mem(f);
    s->irq  = pci_function_irq(f);
    // mem, INTx on
    pci_function_write16(f, 0x04, (pci_function_read16(f, 0x04) | 0x02) & ~0x400);

    LOG("oxpcie952 mmio0=%08x mmio1=%08x irq=%d uarts=%d\n",
        s->bar0, s->bar1, s->irq, *(volatile uint8_t *)(s->bar0 + 4));

    for (int i = 0; i < OXPCIE952_NB_UART; i++) {
        struct uart *u = &s->uart[i];
        u->mmio = (volatile uint8_t *)(s->bar0 + 0x1000 + 0x200 * i);
        u->irq = s->irq;
        ox950_init(u, 115200);
    }
}

#endif
