/* Based on the Linux driver, v6.12
   drivers/net/ethernet/natsemi/natsemi.c
   - ported: dp83815_ee_read, dp83815_get_mac, dp83815_phy_fixup
   - from there too: dsp register values, PME clear, RX size limit,
     full duplex RCFG/TCFG setup, descriptor OWN handling
   - rest from the datasheet below

   Notice from natsemi.c, which this file falls under:

	Written/copyright 1999-2001 by Donald Becker.
	Portions copyright (c) 2001,2002 Sun Microsystems (thockin@sun.com)
	Portions copyright 2001,2002 Manfred Spraul (manfred@colorfullife.com)
	Portions copyright 2004 Harald Welte <laforge@gnumonks.org>

	This software may be used and distributed according to the terms of
	the GNU General Public License (GPL), incorporated herein by reference.
	Drivers based on or derived from this code fall under the GPL and must
	retain the authorship, copyright and license notice.  This file is not
	a complete program and may only be used when the entire operating
	system is licensed under the GPL.  License for under other terms may be
	available.  Contact the original author for details.
*/

#ifndef HW_I686_DP83815
#define HW_I686_DP83815

#include "hw_i686_pci.h"
#include "ethernet_rx.h"

// https://claude.ai/chat/93209308-2baf-42e1-8a88-3594552a998e
// https://www.ti.com/lit/ds/symlink/dp83815.pdf?ts=1778398931364
// md5://84116e37f15283934347cb91a43b37d0

#define dp83815_vendor  0x100b
#define dp83815_device  0x0020


/* DP83815 register offsets (operational, BAR1) */
#define DP83815_CR      0x00    /* Command */
#define DP83815_CFG     0x04    /* Configuration */
#define DP83815_MEAR    0x08    /* EEPROM Access */
#define DP83815_ISR     0x10    /* Interrupt Register -- Read to clear */
#define DP83815_IMR     0x14    /* Interrupt Mask */
#define DP83815_IER     0x18    /* Interrupt Enable */
#define DP83815_TXDP    0x20    /* Tx Descriptor Pointer */
#define DP83815_TCFG    0x24    /* Tx Configuration */
#define DP83815_RXDP    0x30    /* Rx Descriptor Pointer */
#define DP83815_RCFG    0x34    /* Rx Configuration */
#define DP83815_CLKRUN  0x3C    /* Clockrun Control/Status */
#define DP83815_RFCR    0x48    /* Rx Filter Control */
#define DP83815_RFDR    0x4C
#define DP83815_SRR     0x58    /* Silicon Revision */

/* --- Internal PHY registers (memory-mapped, MII layout) --- */
#define DP83815_BMCR    0x80    /* Basic Mode Control (MII reg 0) */
#define DP83815_BMSR    0x84    /* Basic Mode Status  (MII reg 1) */

/* --- DSP registers, behind PGSEL=1 --- */
#define DP83815_PGSEL   0xCC
#define DP83815_PMDCSR  0xE4
#define DP83815_DSPCFG  0xF4
#define DP83815_SDCFG   0xF8
#define DP83815_TSTDAT  0xFC

#define DP83815_PMDCSR_VAL 0x189c
#define DP83815_DSPCFG_VAL 0x5040
#define DP83815_DSPCFG_COEF 0x1000
#define DP83815_SDCFG_VAL  0x008c
#define DP83815_SRR_C      0x0302

/* CR bits */
#define DP83815_CR_TXE  (1 << 0)
#define DP83815_CR_RXE  (1 << 2)
#define DP83815_CR_RST  (1 << 8)

/* CFG bits */
#define DP83815_CFG_FDUP (1 << 29)  /* link is full duplex */

/* MEAR bits */
#define DP83815_MEAR_EEDI  (1 << 0)  /* data to eeprom */
#define DP83815_MEAR_EEDO  (1 << 1)  /* data from eeprom */
#define DP83815_MEAR_EECLK (1 << 2)
#define DP83815_MEAR_EESEL (1 << 3)
#define DP83815_EE_READ    (6 << 6)

/* CLKRUN bits */
#define DP83815_CLKRUN_PMEEN (1 << 8)  /* power management enable */

/* TCFG bits */
#define DP83815_TCFG_CSI (1u << 31) /* carrier sense ignore, full duplex */
#define DP83815_TCFG_HBI (1 << 30)  /* heartbeat ignore, full duplex */
#define DP83815_TCFG_ATP (1 << 28)  /* automatic padding */
#define DP83815_TCFG_ECRETRY (1 << 23)  /* retry after excessive collisions */
#define DP83815_TCFG_MXDMA_256 (7 << 20)  /* max dma burst */
#define DP83815_TCFG_FLTH_512 (16 << 8)   /* fill threshold, 32 byte units */
#define DP83815_TCFG_DRTH_64 2      /* drain threshold, 32 byte units */

/* RCFG bits */
#define DP83815_RCFG_MXDMA_256 (7 << 20)  /* max dma burst */
#define DP83815_RCFG_DRTH_64   (8 << 1)   /* drain threshold, 8 byte units */
#define DP83815_RCFG_ATX (1 << 28)  /* accept transmit packets, full duplex */
#define DP83815_RCFG_ARP (1 << 30)  /* accept runts - off (default fine) */

/* RFCR bits */
#define DP83815_RFCR_RFEN (1u << 31) /* Rx filter enable */
#define DP83815_RFCR_AAB  (1 << 30)  /* accept all broadcast */
#define DP83815_RFCR_APM  (1 << 27)  /* accept perfect match */

/* IMR bits we care about */
#define DP83815_ISR_RXOK  (1 << 0)
#define DP83815_ISR_RXERR (1 << 2)
#define DP83815_ISR_RXDESC (1 << 1)
#define DP83815_ISR_RXIDLE (1 << 4)
#define DP83815_ISR_RXORN  (1 << 5)

#define DP83815_DESC_OK    (1 << 27)   /* "Packet OK" in cmdsts */

/* Descriptor cmdsts bits */
#define DP83815_DESC_OWN  (1u << 31)
#define DP83815_DESC_SIZE_MASK 0xFFF

#define DP83815_NRX     8
#define DP83815_NTX     4
#define DP83815_RXBUF   1536    /* > 1518, 4-byte aligned, 2048 reads as 0 */

/* BMCR */
#define DP83815_BMCR_ANRESTART  0x0200      /* Restart Autoneg (self-clearing) */
#define DP83815_BMCR_ANEN       0x1000      /* Autoneg Enable */
#define DP83815_BMCR_ANEG_KICK  (DP83815_BMCR_ANEN | DP83815_BMCR_ANRESTART)  /* write 0x1200 */




struct dp83815_desc {
    uint32_t link;      /* phys addr of next descriptor */
    uint32_t cmdsts;    /* OWN | flags | size */
    uint32_t bufptr;    /* phys addr of buffer */
    uint32_t pad;       /* keep 16-byte aligned; HW only reads first 3 */
};

struct dp83815 {
    volatile struct dp83815_desc rxring[DP83815_NRX] __attribute__((aligned(16)));
    volatile struct dp83815_desc txring[DP83815_NTX] __attribute__((aligned(16)));
    uint8_t rxbuf[DP83815_NRX][DP83815_RXBUF] __attribute__((aligned(4)));
    uint8_t txbuf[DP83815_NTX][DP83815_RXBUF] __attribute__((aligned(4)));
    uint32_t rx_head;
    uint32_t tx_head;
    uint8_t *mmio;   /* set by your PCI code from BAR1 */
    uint8_t irq;
    struct mac_addr mac;

    void (*rx)(void *, const uint8_t *, uint32_t);
    void *ctx;
};

static inline void dp83815_wr(struct dp83815 *s, uint32_t off, uint32_t v) {
    *(volatile uint32_t *)(s->mmio + off) = v;
}

static inline void dp83815_isr_inner(struct dp83815 *s) {
    uint32_t status = *(volatile uint32_t *)(s->mmio + DP83815_ISR);
    // LOG("[STATUS=%x]\n",status);
    if (!(status & (DP83815_ISR_RXOK |
                    DP83815_ISR_RXDESC |
                    DP83815_ISR_RXERR |
                    DP83815_ISR_RXIDLE |
                    DP83815_ISR_RXORN))) {
        LOG("[?]\n");
        return;
    }
    /* Walk descriptors the chip has handed back (OWN set). */
    while (s->rxring[s->rx_head].cmdsts & DP83815_DESC_OWN) {

        // LOG("[RX %d]\n", s->rx_head);

        uint32_t cs = s->rxring[s->rx_head].cmdsts;
        uint32_t len = cs & DP83815_DESC_SIZE_MASK;

        /* If OK is clear, packet had an error - drop it. */
        if (cs & DP83815_DESC_OK) {
            s->rx(s->ctx, s->rxbuf[s->rx_head], len - 4); // strip FCS
        }

        /* Return descriptor to the chip. */
        s->rxring[s->rx_head].cmdsts = DP83815_RXBUF;
        s->rx_head = (s->rx_head + 1) % DP83815_NRX;
    }

    /* If the chip stopped because it ran out of owned descriptors,
       kick it to re-read the link and resume. */
    if (status & DP83815_ISR_RXIDLE) {
        LOG("[IDLE]\n");
        *(volatile uint32_t *)(s->mmio + DP83815_CR) = DP83815_CR_RXE;
    }
}
static inline void dp83815_reset(struct dp83815 *s) {
    dp83815_wr(s, DP83815_CR, DP83815_CR_RST);
    while (*(volatile uint32_t *)(s->mmio + DP83815_CR) & DP83815_CR_RST)
        ;  /* hardware clears RST when done */
}

static inline void dp83815_write(struct dp83815 *s, uint32_t addr, uint32_t value)  {
    *(volatile uint32_t *)(s->mmio + addr) = value;
}
static inline uint32_t dp83815_read(struct dp83815 *s, uint32_t addr)  {
    return *(volatile uint32_t *)(s->mmio + addr);
}
// Receive Filter read
static inline uint32_t dp83815_rf_read(struct dp83815 *s, uint32_t addr)  {
    // Write address to control register, read data register.
    dp83815_write(s, DP83815_RFCR, addr);
    return dp83815_read(s, DP83815_RFDR);
}

static inline void dp83815_rf_write(struct dp83815 *s, uint32_t addr, uint32_t value)  {
    // Write address to control register, read data register.
    *(volatile uint32_t *)(s->mmio + DP83815_RFCR) = addr;
    *(volatile uint32_t *)(s->mmio + DP83815_RFDR) = value;
}
// Linux natsemi.c eeprom_read, the register read is the delay
static inline uint32_t dp83815_ee_read(struct dp83815 *s, uint32_t addr) {
    uint32_t cmd = addr | DP83815_EE_READ;
    uint32_t val = 0;
    dp83815_wr(s, DP83815_MEAR, DP83815_MEAR_EESEL);
    for (int i = 10; i >= 0; i--) {
        uint32_t bit = DP83815_MEAR_EESEL |
            ((cmd & (1 << i)) ? DP83815_MEAR_EEDI : 0);
        dp83815_wr(s, DP83815_MEAR, bit);
        dp83815_read(s, DP83815_MEAR);
        dp83815_wr(s, DP83815_MEAR, bit | DP83815_MEAR_EECLK);
        dp83815_read(s, DP83815_MEAR);
    }
    dp83815_wr(s, DP83815_MEAR, DP83815_MEAR_EESEL);
    dp83815_read(s, DP83815_MEAR);
    for (int i = 0; i < 16; i++) {
        dp83815_wr(s, DP83815_MEAR, DP83815_MEAR_EESEL | DP83815_MEAR_EECLK);
        dp83815_read(s, DP83815_MEAR);
        if (dp83815_read(s, DP83815_MEAR) & DP83815_MEAR_EEDO) val |= 1 << i;
        dp83815_wr(s, DP83815_MEAR, DP83815_MEAR_EESEL);
        dp83815_read(s, DP83815_MEAR);
    }
    dp83815_wr(s, DP83815_MEAR, DP83815_MEAR_EESEL);
    dp83815_wr(s, DP83815_MEAR, 0);
    return val;
}
// MAC is stored shifted by one bit
static inline void dp83815_get_mac(struct dp83815 *s) {
    uint32_t prev = dp83815_ee_read(s, 6);
    for (int i = 0; i < 3; i++) {
        uint32_t cur = dp83815_ee_read(s, i + 7);
        s->mac.mac[i * 2]     = (cur << 1) + (prev >> 15);
        s->mac.mac[i * 2 + 1] = cur >> 7;
        prev = cur;
    }
    LOG("dp83815 mac: ");
    log_mac(&s->mac);
    LOG("\n");
}
static inline void dp83815_set_mac(struct dp83815 *s) {
    dp83815_rf_write(s, 0, *((uint16_t*)&s->mac.mac[0]));
    dp83815_rf_write(s, 2, *((uint16_t*)&s->mac.mac[2]));
    dp83815_rf_write(s, 4, *((uint16_t*)&s->mac.mac[4]));
}

/* Linux natsemi.c init_phy_fixup
   - some autonegotiations go bad without it
   - dsp needs a few usec after phy start, so retry until it sticks */
static inline void dp83815_phy_fixup(struct dp83815 *s) {
    uint32_t srr = dp83815_read(s, DP83815_SRR);
    for (int i = 0; i < 400; i++) {
        dp83815_wr(s, DP83815_PGSEL, 1);
        dp83815_wr(s, DP83815_PMDCSR, DP83815_PMDCSR_VAL);
        dp83815_wr(s, DP83815_TSTDAT, 0);
        uint32_t dspcfg = srr <= DP83815_SRR_C ? DP83815_DSPCFG_VAL :
            DP83815_DSPCFG_COEF | (dp83815_read(s, DP83815_DSPCFG) & 0xFFFF);
        dp83815_wr(s, DP83815_DSPCFG, dspcfg);
        dp83815_wr(s, DP83815_SDCFG, DP83815_SDCFG_VAL);
        dp83815_wr(s, DP83815_PGSEL, 0);
        for (int j = 0; j < 16; j++) dp83815_read(s, DP83815_CFG); // delay
        dp83815_wr(s, DP83815_PGSEL, 1);
        uint32_t got = dp83815_read(s, DP83815_DSPCFG) & 0xFFFF;
        dp83815_wr(s, DP83815_PGSEL, 0);
        if (got == dspcfg) {
            LOG("dp83815 srr=%x dspcfg=%x tries=%d\n", srr, got, i + 1);
            return;
        }
    }
    LOG("dp83815 dsp fixup failed\n");
}

/* Link comes up after init, so this is called again on transmit. */
static inline void dp83815_duplex(struct dp83815 *s) {
    uint32_t tcfg = DP83815_TCFG_ATP | DP83815_TCFG_ECRETRY |
        DP83815_TCFG_MXDMA_256 | DP83815_TCFG_FLTH_512 | DP83815_TCFG_DRTH_64;
    uint32_t rcfg = DP83815_RCFG_MXDMA_256 | DP83815_RCFG_DRTH_64;
    if (dp83815_read(s, DP83815_CFG) & DP83815_CFG_FDUP) {
        tcfg |= DP83815_TCFG_CSI | DP83815_TCFG_HBI;
        rcfg |= DP83815_RCFG_ATX;
    }
    dp83815_wr(s, DP83815_TCFG, tcfg);
    dp83815_wr(s, DP83815_RCFG, rcfg);
}

/* Returns 0 on success, -1 if no descriptor is free or packet too big. */
static inline int dp83815_transmit(struct dp83815 *s, const void *data, uint16_t len) {
    volatile struct dp83815_desc *d = &s->txring[s->tx_head];
    if (len > sizeof(s->txbuf[0])) return -1;
    if (d->cmdsts & DP83815_DESC_OWN) return -1;

    memcpy(s->txbuf[s->tx_head], data, len);
    dp83815_duplex(s);
    __asm__ volatile ("" ::: "memory");  // OWN goes last

    /* For tx the driver is the producer, the chip clears OWN when done. */
    d->cmdsts = DP83815_DESC_OWN | len;
    s->tx_head = (s->tx_head + 1) % DP83815_NTX;
    dp83815_wr(s, DP83815_CR, DP83815_CR_TXE);
    return 0;
}

static inline void dp83815_rx_ignore(void *s, const uint8_t *data, uint32_t len) {
}

static inline void dp83815_init(struct dp83815 *s,
                                const struct pci_function *f) {

    // Only one instance, soekris net4501 has 3 chips: first one wins
    if (s->mmio) {
        LOG("dp83815 %02x:%02x.%d ignored\n", f->bus, f->dev, f->func);
        return;
    }

    s->mmio = (uint8_t*)(pci_function_read32(f, PCI_CFG_BAR1) & ~0xF);
    s->irq  = pci_function_read32(f, PCI_CFG_IRQ) & 0xFF;
    s->rx = dp83815_rx_ignore;
    LOG("dp83815 mmio=%08x irq=%d\n", s->mmio, s->irq);

    // Before the first mmio access
    uint16_t cmd = pci_function_read16(f, 0x04);
    /* want bits: 0x02 (memory space) | 0x04 (bus master) set,
       0x400 (interrupt disable) clear */
    pci_function_write16(f, 0x04, cmd | 0x06);

    // Read mac address (test card is 00:02:E3:0B:8D:BA)

    // From EEPROM: the receive filter is zero on net4801 after PXE
    dp83815_get_mac(s);
    dp83815_reset(s);

    // But after reset it is set to zero.  So maybe just don't reset
    // the chip?  What else is deleted?
    dp83815_set_mac(s);
    //dp83815_get_mac(s);

    dp83815_phy_fixup(s);

    // Chip writes nothing to memory with PME set, EEPROM can set it
    dp83815_wr(s, DP83815_CLKRUN,
               dp83815_read(s, DP83815_CLKRUN) & ~DP83815_CLKRUN_PMEEN);

    s->rx_head = 0;

    /* Build a circular ring of descriptors, all owned by the chip,
       each pointing at its buffer.  For rx the chip is the
       producer: it sets OWN, the driver clears it. */
    for (int i = 0; i < DP83815_NRX; i++) {
        s->rxring[i].link   = (uint32_t)(uintptr_t)&s->rxring[(i + 1) % DP83815_NRX];
        s->rxring[i].bufptr = (uint32_t)(uintptr_t)s->rxbuf[i];
        s->rxring[i].cmdsts = DP83815_RXBUF; /* hand to chip, max size */
    }

    s->tx_head = 0;
    for (int i = 0; i < DP83815_NTX; i++) {
        s->txring[i].link   = (uint32_t)(uintptr_t)&s->txring[(i + 1) % DP83815_NTX];
        s->txring[i].bufptr = (uint32_t)(uintptr_t)s->txbuf[i];
        s->txring[i].cmdsts = 0;
    }

    /* Point the chip at descriptor 0. CR.RXE must be 0 here (it is, post-reset). */
    dp83815_wr(s, DP83815_RXDP, (uint32_t)(uintptr_t)&s->rxring[0]);
    dp83815_wr(s, DP83815_TXDP, (uint32_t)(uintptr_t)&s->txring[0]);

    /* Filter: accept broadcast + perfect-match unicast. Assumes MAC was
       already loaded from EEPROM at reset; otherwise program via RFCR/RFDR. */
    dp83815_wr(s, DP83815_RFCR,
               DP83815_RFCR_RFEN |
               DP83815_RFCR_AAB |
               DP83815_RFCR_APM);

    /* Rx config: drop runts, drop errors, no loopback.  DRTH 0 puts an
       empty MORE descriptor in front of each packet. */
    dp83815_duplex(s);

    /* Unmask the Rx interrupts we want, then globally enable. */
    dp83815_wr(s, DP83815_IMR,
               DP83815_ISR_RXOK |
               DP83815_ISR_RXERR |
               DP83815_ISR_RXDESC |
               DP83815_ISR_RXIDLE |  // idle can follow the status read
               DP83815_ISR_RXORN);
    dp83815_wr(s, DP83815_IER, 1);

    /* Go. */
    dp83815_wr(s, DP83815_CR, DP83815_CR_RXE);

    // Kick autoneg
    // dp83815_wr(s, DP83815_BMCR, DP83815_BMCR_ANEG_KICK); 


}

#endif
