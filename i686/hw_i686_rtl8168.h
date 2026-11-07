/* Realtek RTL8168 family, PCIe gigabit
   Based on iPXE src/drivers/net/realtek.c and realtek.h
   - ported: reset, C+ mode setup, ring layout, TCR/RCR values
   - no phy access: the link stays as the boot firmware left it
   - 32 bit addresses only, I/O BAR, INTx

   Notice from realtek.c, which this file falls under:

	Copyright (C) 2012 Michael Brown <mbrown@fensystems.co.uk>.

	This program is free software; you can redistribute it and/or
	modify it under the terms of the GNU General Public License as
	published by the Free Software Foundation; either version 2 of the
	License, or (at your option) any later version.

	This program is distributed in the hope that it will be useful, but
	WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
	General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program; if not, write to the Free Software
	Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
	02110-1301, USA.

	You can also choose to distribute this program under the terms of
	the Unmodified Binary Distribution Licence (as given in the file
	COPYING.UBDL), provided that you have satisfied its requirements.
*/

#ifndef HW_I686_RTL8168
#define HW_I686_RTL8168

#include "hw_i686_pci.h"
#include "ethernet_rx.h"

#define rtl8168_vendor  0x10ec
#define rtl8168_device  0x8168

/* Registers, all below 0x100 so the I/O BAR reaches them */
#define RTL8168_IDR0    0x00
#define RTL8168_MAR0    0x08
#define RTL8168_TNPDS   0x20  /* Tx ring, 64 bit */
#define RTL8168_CR      0x37
#define RTL8168_TPPOLL  0x38
#define RTL8168_IMR     0x3C
#define RTL8168_ISR     0x3E
#define RTL8168_TCR     0x40
#define RTL8168_RCR     0x44
#define RTL8168_PHYSTATUS 0x6C
#define RTL8168_RMS     0xDA
#define RTL8168_CPCR    0xE0
#define RTL8168_RDSAR   0xE4  /* Rx ring, 64 bit */

#define RTL8168_CR_RST  0x10
#define RTL8168_CR_RE   0x08
#define RTL8168_CR_TE   0x04

#define RTL8168_TPPOLL_NPQ 0x40

/* Interrupt bits */
#define RTL8168_IRQ_ROK  0x0001
#define RTL8168_IRQ_RER  0x0002
#define RTL8168_IRQ_RDU  0x0010  /* Rx ring empty */
#define RTL8168_IRQ_FOVW 0x0040  /* Rx fifo overflow */
#define RTL8168_IRQS (RTL8168_IRQ_ROK | RTL8168_IRQ_RER | \
                      RTL8168_IRQ_RDU | RTL8168_IRQ_FOVW)

#define RTL8168_TCR_MXDMA  (7 << 8)   /* Unlimited */

#define RTL8168_RCR_APM    0x00000002
#define RTL8168_RCR_AM     0x00000004
#define RTL8168_RCR_AB     0x00000008
#define RTL8168_RCR_WRAP   0x00000080
#define RTL8168_RCR_MXDMA  (7 << 8)   /* Unlimited */
#define RTL8168_RCR_RBLEN  (3 << 11)  /* mask */
#define RTL8168_RCR_RXFTH  (7 << 13)  /* Whole packet */
#define RTL8168_RCR_STOP_WORKING 0x01000000  /* iPXE: here be dragons */

#define RTL8168_CPCR_CPTX  0x0001
#define RTL8168_CPCR_CPRX  0x0002
#define RTL8168_CPCR_MULRW 0x0008
#define RTL8168_CPCR_VLAN  0x0040

/* Descriptor bits */
#define RTL8168_DESC_OWN   0x8000
#define RTL8168_DESC_EOR   0x4000
#define RTL8168_DESC_FS    0x2000
#define RTL8168_DESC_LS    0x1000
#define RTL8168_DESC_RES   0x0020  /* Rx error summary */
#define RTL8168_DESC_LEN   0x3FFF

/* Rings are 256 byte aligned */
#define RTL8168_NRX    16
#define RTL8168_NTX    8
#define RTL8168_BUF    1536
#define RTL8168_ZLEN   60

struct rtl8168_desc {
    uint16_t length;
    uint16_t flags;
    uint32_t reserved;
    uint32_t addr_lo;
    uint32_t addr_hi;
};

struct rtl8168 {
    volatile struct rtl8168_desc rxring[RTL8168_NRX] __attribute__((aligned(256)));
    volatile struct rtl8168_desc txring[RTL8168_NTX] __attribute__((aligned(256)));
    uint8_t rxbuf[RTL8168_NRX][RTL8168_BUF] __attribute__((aligned(8)));
    uint8_t txbuf[RTL8168_NTX][RTL8168_BUF] __attribute__((aligned(8)));
    uint32_t rx_head;
    uint32_t tx_head;
    uint32_t iobase;
    uint8_t irq;
    struct mac_addr mac;

    void (*rx)(void *, const uint8_t *, uint32_t);
    void *ctx;

    uint32_t nb_rx;    // passed to rx
    uint32_t nb_rdu;   // ring empty interrupts
    uint32_t nb_fovw;  // fifo overflow interrupts
};

static inline void rtl8168_rx_give(struct rtl8168 *s, uint32_t i) {
    volatile struct rtl8168_desc *d = &s->rxring[i];
    d->length = RTL8168_BUF;
    __asm__ volatile ("" ::: "memory");  // flags go last
    d->flags = RTL8168_DESC_OWN |
        (i == RTL8168_NRX - 1 ? RTL8168_DESC_EOR : 0);
}

/* Masked while running, so anything that comes in after the ack makes
   a new edge at the end. */
static inline void rtl8168_isr_inner(struct rtl8168 *s) {
    outw(s->iobase + RTL8168_IMR, 0);
    uint16_t status = inw(s->iobase + RTL8168_ISR);
    outw(s->iobase + RTL8168_ISR, status);
    if (status & RTL8168_IRQ_RDU)  s->nb_rdu++;
    if (status & RTL8168_IRQ_FOVW) s->nb_fovw++;
    while (!(s->rxring[s->rx_head].flags & RTL8168_DESC_OWN)) {
        volatile struct rtl8168_desc *d = &s->rxring[s->rx_head];
        if (!(d->flags & RTL8168_DESC_RES)) {
            s->rx(s->ctx, s->rxbuf[s->rx_head],
                  (d->length & RTL8168_DESC_LEN) - 4);  // strip crc
            s->nb_rx++;
        }
        rtl8168_rx_give(s, s->rx_head);
        s->rx_head = (s->rx_head + 1) % RTL8168_NRX;
    }
    outw(s->iobase + RTL8168_IMR, RTL8168_IRQS);
}

/* Returns 0 on success, -1 if no descriptor is free or packet too big. */
static inline int rtl8168_transmit(struct rtl8168 *s, const void *data, uint16_t len) {
    volatile struct rtl8168_desc *d = &s->txring[s->tx_head];
    if (len > sizeof(s->txbuf[0])) return -1;
    if (d->flags & RTL8168_DESC_OWN) return -1;

    memcpy(s->txbuf[s->tx_head], data, len);
    if (len < RTL8168_ZLEN) {
        memset(s->txbuf[s->tx_head] + len, 0, RTL8168_ZLEN - len);
        len = RTL8168_ZLEN;
    }
    d->length = len;
    __asm__ volatile ("" ::: "memory");  // flags go last
    d->flags = RTL8168_DESC_OWN | RTL8168_DESC_FS | RTL8168_DESC_LS |
        (s->tx_head == RTL8168_NTX - 1 ? RTL8168_DESC_EOR : 0);
    __asm__ volatile ("" ::: "memory");

    s->tx_head = (s->tx_head + 1) % RTL8168_NTX;
    outb(s->iobase + RTL8168_TPPOLL, RTL8168_TPPOLL_NPQ);
    return 0;
}

/* Totals since init. */
static inline void rtl8168_stats(struct rtl8168 *s) {
    LOG("rx %d no_buf %d overrun %d phystatus %02x\n",
        s->nb_rx, s->nb_rdu, s->nb_fovw,
        inb(s->iobase + RTL8168_PHYSTATUS));
}

static inline void rtl8168_rx_ignore(void *s, const uint8_t *data, uint32_t len) {
}

static inline void rtl8168_reset(struct rtl8168 *s) {
    outb(s->iobase + RTL8168_CR, RTL8168_CR_RST);
    for (int i = 0; i < 1000000; i++) {
        if (!(inb(s->iobase + RTL8168_CR) & RTL8168_CR_RST)) return;
    }
    LOG("rtl8168 reset timeout\n");
}

static inline void rtl8168_init(struct rtl8168 *s,
                                const struct pci_function *f) {

    s->iobase = pci_function_bar0_io(f);
    s->irq    = pci_function_irq(f);
    s->rx = rtl8168_rx_ignore;

    // I/O space, bus master, INTx: linux leaves that off when using msi
    pci_function_write16(f, 0x04, (pci_function_read16(f, 0x04) | 0x05) & ~0x400);

    rtl8168_reset(s);

    for (int i = 0; i < 6; i++) {
        s->mac.mac[i] = inb(s->iobase + RTL8168_IDR0 + i);
    }
    LOG("rtl8168 io=%04x irq=%d xid=%03x mac: ", s->iobase, s->irq,
        (inl(s->iobase + RTL8168_TCR) >> 20) & 0xFCF);
    log_mac(&s->mac);
    LOG("\n");

    outw(s->iobase + RTL8168_RMS, RTL8168_BUF);
    outw(s->iobase + RTL8168_CPCR,
         (inw(s->iobase + RTL8168_CPCR) & ~RTL8168_CPCR_VLAN) |
         RTL8168_CPCR_MULRW | RTL8168_CPCR_CPRX | RTL8168_CPCR_CPTX);

    s->tx_head = 0;
    for (int i = 0; i < RTL8168_NTX; i++) {
        s->txring[i].addr_lo = (uint32_t)(uintptr_t)s->txbuf[i];
        s->txring[i].addr_hi = 0;
        s->txring[i].flags   = 0;
    }
    s->rx_head = 0;
    for (int i = 0; i < RTL8168_NRX; i++) {
        s->rxring[i].addr_lo = (uint32_t)(uintptr_t)s->rxbuf[i];
        s->rxring[i].addr_hi = 0;
        rtl8168_rx_give(s, i);
    }
    outl(s->iobase + RTL8168_TNPDS + 4, 0);
    outl(s->iobase + RTL8168_TNPDS, (uint32_t)(uintptr_t)s->txring);
    outl(s->iobase + RTL8168_RDSAR + 4, 0);
    outl(s->iobase + RTL8168_RDSAR, (uint32_t)(uintptr_t)s->rxring);

    outl(s->iobase + RTL8168_MAR0,     0xFFFFFFFF);
    outl(s->iobase + RTL8168_MAR0 + 4, 0xFFFFFFFF);

    outb(s->iobase + RTL8168_CR, RTL8168_CR_TE | RTL8168_CR_RE);
    outl(s->iobase + RTL8168_TCR,
         inl(s->iobase + RTL8168_TCR) | RTL8168_TCR_MXDMA);
    outl(s->iobase + RTL8168_RCR,
         (inl(s->iobase + RTL8168_RCR) &
          ~(RTL8168_RCR_STOP_WORKING | RTL8168_RCR_RBLEN)) |
         RTL8168_RCR_RXFTH | RTL8168_RCR_MXDMA | RTL8168_RCR_WRAP |
         RTL8168_RCR_AB | RTL8168_RCR_AM | RTL8168_RCR_APM);

    outw(s->iobase + RTL8168_IMR, RTL8168_IRQS);
}

#endif
