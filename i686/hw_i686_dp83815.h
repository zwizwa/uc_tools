#ifndef HW_I686_DP83815
#define HW_I686_DP83815

#include "hw_i686_pci.h"

// https://claude.ai/chat/93209308-2baf-42e1-8a88-3594552a998e
// https://www.ti.com/lit/ds/symlink/dp83815.pdf?ts=1778398931364
// md5://84116e37f15283934347cb91a43b37d0

#define DP83815_VENDOR  0x100b
#define DP83815_DEVICE  0x0020


/* DP83815 register offsets (operational, BAR1) */
#define DP83815_CR      0x00    /* Command */
#define DP83815_ISR     0x10    /* Interrupt Register -- Read to clear */
#define DP83815_IMR     0x14    /* Interrupt Mask */
#define DP83815_IER     0x18    /* Interrupt Enable */
#define DP83815_RXDP    0x30    /* Rx Descriptor Pointer */
#define DP83815_RCFG    0x34    /* Rx Configuration */
#define DP83815_RFCR    0x48    /* Rx Filter Control */
#define DP83815_RFDR    0x4C

/* CR bits */
#define DP83815_CR_RXE  (1 << 2)
#define DP83815_CR_RST  (1 << 8)

/* RCFG bits */
#define DP83815_RCFG_ATX (1 << 28)  /* accept transmit packets (loopback) - off */
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

#define DP83815_DESC_OK    (1 << 25)   /* "Packet OK" in cmdsts */

/* Descriptor cmdsts bits */
#define DP83815_DESC_OWN  (1u << 31)
#define DP83815_DESC_SIZE_MASK 0xFFF

#define DP83815_NRX     8
#define DP83815_RXBUF   2048    /* > 1518, 4-byte aligned */

struct dp83815_desc {
    uint32_t link;      /* phys addr of next descriptor */
    uint32_t cmdsts;    /* OWN | flags | size */
    uint32_t bufptr;    /* phys addr of buffer */
    uint32_t pad;       /* keep 16-byte aligned; HW only reads first 3 */
};

struct dp83815 {
    volatile struct dp83815_desc rxring[DP83815_NRX] __attribute__((aligned(16)));
    uint8_t rxbuf[DP83815_NRX][DP83815_RXBUF] __attribute__((aligned(4)));
    uint32_t rx_head;
    uint8_t *mmio;   /* set by your PCI code from BAR1 */
    uint8_t irq;
    uint8_t mac[6];
};

static inline void dp83815_wr(struct dp83815 *s, uint32_t off, uint32_t v) {
    *(volatile uint32_t *)(s->mmio + off) = v;
}

static inline void dp83815_isr_inner(struct dp83815 *s) {
    uint32_t status = *(volatile uint32_t *)(s->mmio + DP83815_ISR);
    LOG("[STATUS=%x]\n",status);
    if (!(status & (DP83815_ISR_RXOK |
                    DP83815_ISR_RXDESC |
                    DP83815_ISR_RXERR))) {
        LOG("[?]\n");
        return;
    }
    /* Walk descriptors the chip has handed back (OWN cleared). */
    while (!(s->rxring[s->rx_head].cmdsts & DP83815_DESC_OWN)) {

        LOG("[RX %d]\n", s->rx_head);

        uint32_t cs = s->rxring[s->rx_head].cmdsts;
        uint32_t len = cs & DP83815_DESC_SIZE_MASK;
        (void)len;

        /* OK bit is 25; if clear, packet had an error - drop it. */
        if (!(cs & 0x047F0000)) {
            /* Hand buffer + length up to your stack here.
               len includes the 4-byte FCS, subtract if you don't want it. */
            // net_rx(rxbuf[rx_head], len - 4);
        }

        /* Return descriptor to the chip. */
        s->rxring[s->rx_head].cmdsts = DP83815_DESC_OWN | DP83815_RXBUF;
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
static inline void dp83815_get_mac(struct dp83815 *s) {
    *((uint16_t*)&s->mac[0]) = dp83815_rf_read(s, 0);
    *((uint16_t*)&s->mac[2]) = dp83815_rf_read(s, 2);
    *((uint16_t*)&s->mac[4]) = dp83815_rf_read(s, 4);
    LOG("dp83815 mac %02x:%02x:%02x:%02x:%02x:%02x\n",
        s->mac[0],s->mac[1],s->mac[2],
        s->mac[3],s->mac[4],s->mac[5]);
}
static inline void dp83815_set_mac(struct dp83815 *s) {
    dp83815_rf_write(s, 0, *((uint16_t*)&s->mac[0]));
    dp83815_rf_write(s, 2, *((uint16_t*)&s->mac[2]));
    dp83815_rf_write(s, 4, *((uint16_t*)&s->mac[4]));
}

static inline void dp83815_init(struct dp83815 *s,
                                const struct pci_function *f) {

    s->mmio = (uint8_t*)(pci_function_read32(f, PCI_CFG_BAR1) & ~0xF);
    s->irq  = pci_function_read32(f, PCI_CFG_IRQ) & 0xFF;
    LOG("dp83815 mmio=%08x irq=%d\n", s->mmio, s->irq);


    // Read mac address (test card is 00:02:E3:0B:8D:BA)

    // Note that iPXE reads the EEPROM. I don't want to implement the
    // bitbang that is necessary for that, but it might be more
    // robust.  MAC address should be loaded into receive filter after
    // boot and after iPXE.  Both are verified on my machine.
    dp83815_get_mac(s);
    dp83815_reset(s);

    // But after reset it is set to zero.  So maybe just don't reset
    // the chip?  What else is deleted?
    dp83815_set_mac(s);
    dp83815_get_mac(s);

    uint16_t cmd = pci_function_read16(f, 0x04);
    /* want bits: 0x02 (memory space) | 0x04 (bus master) set,
       0x400 (interrupt disable) clear */
    pci_function_write16(f, 0x04, cmd | 0x06);

    s->rx_head = 0;

    /* Build a circular ring of descriptors, all owned by the chip,
       each pointing at its 2KB buffer. */
    for (int i = 0; i < DP83815_NRX; i++) {
        s->rxring[i].link   = (uint32_t)(uintptr_t)&s->rxring[(i + 1) % DP83815_NRX];
        s->rxring[i].bufptr = (uint32_t)(uintptr_t)s->rxbuf[i];
        s->rxring[i].cmdsts = DP83815_DESC_OWN | DP83815_RXBUF; /* hand to chip, max size */
    }

    /* Point the chip at descriptor 0. CR.RXE must be 0 here (it is, post-reset). */
    dp83815_wr(s, DP83815_RXDP, (uint32_t)(uintptr_t)&s->rxring[0]);

    /* Filter: accept broadcast + perfect-match unicast. Assumes MAC was
       already loaded from EEPROM at reset; otherwise program via RFCR/RFDR. */
    dp83815_wr(s, DP83815_RFCR,
               DP83815_RFCR_RFEN |
               DP83815_RFCR_AAB |
               DP83815_RFCR_APM);

    /* Rx config: defaults are fine (drop runts, drop errors, no loopback). */
    dp83815_wr(s, DP83815_RCFG, 0);

    /* Unmask the Rx interrupts we want, then globally enable. */
    dp83815_wr(s, DP83815_IMR,
               DP83815_ISR_RXOK |
               DP83815_ISR_RXERR |
               DP83815_ISR_RXDESC);
    dp83815_wr(s, DP83815_IER, 1);

    /* Go. */
    dp83815_wr(s, DP83815_CR, DP83815_CR_RXE);
}

#endif
