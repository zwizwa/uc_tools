/* Intel 82574L, 82579LM and 82566DM, PCIe gigabit
   Based on iPXE src/drivers/net/intel.c and intel.h
   - ported: reset order, ring setup, TCTL/RCTL values, register and
     bit definitions
   - EECD AUTO_RD wait, stats registers and 82579 SWFLAG are from Linux
     drivers/net/ethernet/intel/e1000e, GPL2
   - 32 bit addresses only, legacy descriptors, INTx
   - 82566DM links at 100 only: phy falls back from gig

   Notice from intel.c, which this file falls under:

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

#ifndef HW_I686_I82574
#define HW_I686_I82574

#include "hw_i686_pci.h"
#include "ethernet_rx.h"

#define i82574_vendor  0x8086
#define i82574_device  0x10d3
#define i82579_device  0x1502
#define i82566_device  0x104a

/* Registers, BAR0 */
#define I82574_CTRL    0x00000
#define I82574_STATUS  0x00008
#define I82574_EECD    0x00010
#define I82574_ICR     0x000C0  /* Read to clear */
#define I82574_IMS     0x000D0
#define I82574_RCTL    0x00100
#define I82574_TCTL    0x00400
#define I82574_EXTCNF_CTRL 0x00F00
#define I82574_PBA     0x01000
#define I82574_PBS     0x01008
#define I82574_RD      0x02800  /* Rx ring block */
#define I82574_TD      0x03800  /* Tx ring block */
#define I82574_MPC     0x04010  /* Missed packets, stats are read to clear */
#define I82574_GPRC    0x04074  /* Good packets received */
#define I82574_GPTC    0x04080  /* Good packets transmitted */
#define I82574_RNBC    0x040A0  /* Receive no buffers */
#define I82574_RAL0    0x05400
#define I82574_RAH0    0x05404

/* Ring block offsets */
#define I82574_xDBAL   0x00
#define I82574_xDBAH   0x04
#define I82574_xDLEN   0x08
#define I82574_xDH     0x10
#define I82574_xDT     0x18
#define I82574_xDCTL   0x28

#define I82574_xDCTL_ENABLE (1 << 25)

/* CTRL bits */
#define I82574_CTRL_LRST    (1 << 3)
#define I82574_CTRL_ASDE    (1 << 5)
#define I82574_CTRL_SLU     (1 << 6)
#define I82574_CTRL_FRCSPD  (1 << 11)
#define I82574_CTRL_FRCDPLX (1 << 12)
#define I82574_CTRL_RST     (1 << 26)
#define I82574_CTRL_PHY_RST (1u << 31)

#define I82574_STATUS_LU    (1 << 1)
#define I82574_EECD_AUTO_RD (1 << 9)  /* nvm loaded, RAL0 valid */
#define I82574_EXTCNF_SWFLAG (1 << 5)

/* Interrupt bits */
#define I82574_IRQ_RXDMT0   (1 << 4)
#define I82574_IRQ_RXO      (1 << 6)
#define I82574_IRQ_RXT0     (1 << 7)

/* RCTL bits, BSIZE 0 is 2048 */
#define I82574_RCTL_EN      (1 << 1)
#define I82574_RCTL_BAM     (1 << 15)
#define I82574_RCTL_SECRC   (1 << 26)

/* TCTL bits */
#define I82574_TCTL_EN      (1 << 1)
#define I82574_TCTL_PSP     (1 << 3)
#define I82574_TCTL_CT      (0x0F << 4)
#define I82574_TCTL_COLD    (0x40 << 12)
#define I82574_TCTL_CT_COLD 0x003FFFF0  /* mask */

/* Descriptor bits */
#define I82574_CMD_EOP      0x01
#define I82574_CMD_IFCS     0x02
#define I82574_CMD_RS       0x08
#define I82574_STATUS_DD    0x01
#define I82574_RX_ERRORS    0xFF00  /* in status */

/* Ring size is a multiple of 128 bytes */
#define I82574_NRX     8
#define I82574_NTX     8
#define I82574_RXBUF   2048

struct i82574_desc {
    uint32_t addr_lo;
    uint32_t addr_hi;
    uint16_t length;
    uint8_t  flags;
    uint8_t  cmd;
    uint32_t status;
};

struct i82574 {
    volatile struct i82574_desc rxring[I82574_NRX] __attribute__((aligned(128)));
    volatile struct i82574_desc txring[I82574_NTX] __attribute__((aligned(128)));
    uint8_t rxbuf[I82574_NRX][I82574_RXBUF] __attribute__((aligned(4)));
    uint8_t txbuf[I82574_NTX][I82574_RXBUF] __attribute__((aligned(4)));
    uint32_t rx_head;
    uint32_t tx_head;
    uint8_t *mmio;
    uint8_t irq;
    uint8_t ich;      // 82579, 82566: phy is shared with the ME
    uint8_t ich8;     // 82566
    struct mac_addr mac;

    void (*rx)(void *, const uint8_t *, uint32_t);
    void *ctx;

    uint32_t nb_rx;   // passed to rx
    uint32_t nb_rxo;  // overrun interrupts
    uint32_t mpc, gprc, gptc, rnbc;
};

static inline void i82574_wr(struct i82574 *s, uint32_t off, uint32_t v) {
    *(volatile uint32_t *)(s->mmio + off) = v;
}
static inline uint32_t i82574_rd(struct i82574 *s, uint32_t off) {
    return *(volatile uint32_t *)(s->mmio + off);
}

static inline void i82574_isr_inner(struct i82574 *s) {
    uint32_t status = i82574_rd(s, I82574_ICR);
    if (!status) {
        LOG("[?]\n");
        return;
    }
    if (status & I82574_IRQ_RXO) s->nb_rxo++;
    while (s->rxring[s->rx_head].status & I82574_STATUS_DD) {
        volatile struct i82574_desc *d = &s->rxring[s->rx_head];
        if (!(d->status & I82574_RX_ERRORS)) {
            s->rx(s->ctx, s->rxbuf[s->rx_head], d->length);
            s->nb_rx++;
        }
        /* Tail trails head, the chip owns everything in between. */
        d->status = 0;
        i82574_wr(s, I82574_RD + I82574_xDT, s->rx_head);
        s->rx_head = (s->rx_head + 1) % I82574_NRX;
    }
}

/* Returns 0 on success, -1 if no descriptor is free or packet too big. */
static inline int i82574_transmit(struct i82574 *s, const void *data, uint16_t len) {
    uint32_t next = (s->tx_head + 1) % I82574_NTX;
    volatile struct i82574_desc *d = &s->txring[s->tx_head];
    if (len > sizeof(s->txbuf[0])) return -1;
    // Tail must not reach head: that reads as an empty ring
    if (!(s->txring[next].status & I82574_STATUS_DD)) return -1;

    memcpy(s->txbuf[s->tx_head], data, len);
    d->length = len;
    d->cmd    = I82574_CMD_EOP | I82574_CMD_IFCS | I82574_CMD_RS;
    d->status = 0;
    __asm__ volatile ("" ::: "memory");  // tail goes last

    s->tx_head = next;
    i82574_wr(s, I82574_TD + I82574_xDT, next);
    return 0;
}

/* Totals since init. */
static inline void i82574_stats(struct i82574 *s) {
    s->mpc  += i82574_rd(s, I82574_MPC);
    s->gprc += i82574_rd(s, I82574_GPRC);
    s->gptc += i82574_rd(s, I82574_GPTC);
    s->rnbc += i82574_rd(s, I82574_RNBC);
    LOG("rx %d good %d missed %d no_buf %d overrun %d tx %d\n",
        s->nb_rx, s->gprc, s->mpc, s->rnbc, s->nb_rxo, s->gptc);
}

static inline void i82574_rx_ignore(void *s, const uint8_t *data, uint32_t len) {
}

/* Self-clearing, the nvm load after it is the longest part. */
static inline void i82574_reset_wait(struct i82574 *s) {
    for (int i = 0; i < 1000000; i++) {
        if (!(i82574_rd(s, I82574_CTRL) & I82574_CTRL_RST) &&
            (s->ich8 || (i82574_rd(s, I82574_EECD) & I82574_EECD_AUTO_RD))) return;
    }
    LOG("i82574 reset timeout\n");
}

/* Arbitrates with the ME, the reset clears it. */
static inline void i82574_swflag(struct i82574 *s) {
    for (int i = 0; i < 100000; i++) {
        i82574_wr(s, I82574_EXTCNF_CTRL, i82574_rd(s, I82574_EXTCNF_CTRL) |
                  I82574_EXTCNF_SWFLAG);
        if (i82574_rd(s, I82574_EXTCNF_CTRL) & I82574_EXTCNF_SWFLAG) return;
    }
    LOG("i82574 swflag timeout\n");
}

/* e1000e: mmio right after reset hangs it.  About 20 ms. */
static inline void i82574_ich_delay(struct i82574 *s) {
    if (s->ich) for (int i = 0; i < 40000; i++) inb(0x80);
}

/* A previous driver can leave the phy powered down, only a phy reset
   gets it back.  Skipped with link up: that is a slow renegotiation.
   82579: mac reset only, iPXE: phy reset breaks the link speed. */
static inline void i82574_reset(struct i82574 *s) {
    uint32_t ctrl = i82574_rd(s, I82574_CTRL);
    if (s->ich) i82574_swflag(s);
    if (s->ich8) {  // iPXE: packet buffer errata
        i82574_wr(s, I82574_PBA, 0x08);
        i82574_wr(s, I82574_PBS, 0x10);
    }
    i82574_wr(s, I82574_CTRL, ctrl | I82574_CTRL_RST);
    i82574_ich_delay(s);
    i82574_reset_wait(s);

    ctrl |= I82574_CTRL_ASDE | I82574_CTRL_SLU;
    ctrl &= ~(I82574_CTRL_LRST | I82574_CTRL_FRCSPD | I82574_CTRL_FRCDPLX |
              I82574_CTRL_RST | I82574_CTRL_PHY_RST);
    i82574_wr(s, I82574_CTRL, ctrl);
    i82574_ich_delay(s);
    if ((s->ich && !s->ich8) ||
        (i82574_rd(s, I82574_STATUS) & I82574_STATUS_LU)) return;

    if (s->ich) i82574_swflag(s);
    i82574_wr(s, I82574_CTRL, ctrl | I82574_CTRL_RST | I82574_CTRL_PHY_RST);
    i82574_ich_delay(s);
    i82574_reset_wait(s);
    for (int i = 0; i < 1000; i++) i82574_rd(s, I82574_STATUS); // delay
    i82574_wr(s, I82574_CTRL, ctrl);  // phy reset is not self-clearing
}

static inline void i82574_get_mac(struct i82574 *s) {
    uint32_t lo = i82574_rd(s, I82574_RAL0);
    uint32_t hi = i82574_rd(s, I82574_RAH0);
    memcpy(&s->mac.mac[0], &lo, 4);
    memcpy(&s->mac.mac[4], &hi, 2);
    LOG("i82574 mac: ");
    log_mac(&s->mac);
    LOG("\n");
}

static inline void i82574_ring(struct i82574 *s, uint32_t reg,
                               volatile struct i82574_desc *ring, uint32_t len) {
    i82574_wr(s, reg + I82574_xDBAL, (uint32_t)(uintptr_t)ring);
    i82574_wr(s, reg + I82574_xDBAH, 0);
    i82574_wr(s, reg + I82574_xDLEN, len);
    i82574_wr(s, reg + I82574_xDH, 0);
    i82574_wr(s, reg + I82574_xDT, 0);
    i82574_wr(s, reg + I82574_xDCTL,
              i82574_rd(s, reg + I82574_xDCTL) | I82574_xDCTL_ENABLE);
}

static inline void i82574_init(struct i82574 *s,
                               const struct pci_function *f) {

    s->mmio = (uint8_t*)pci_function_bar0_mem(f);
    s->irq  = pci_function_irq(f);
    s->ich8 = f->device == i82566_device;
    s->ich  = s->ich8 || f->device == i82579_device;
    s->rx = i82574_rx_ignore;
    LOG("i82574 mmio=%08x irq=%d\n", s->mmio, s->irq);

    // Before the first mmio access: memory space, bus master
    pci_function_write16(f, 0x04, pci_function_read16(f, 0x04) | 0x06);

    i82574_reset(s);
    i82574_get_mac(s);

    s->rx_head = 0;
    for (int i = 0; i < I82574_NRX; i++) {
        s->rxring[i].addr_lo = (uint32_t)(uintptr_t)s->rxbuf[i];
        s->rxring[i].addr_hi = 0;
        s->rxring[i].status  = 0;
    }
    /* Marked done, so transmit sees them as free. */
    s->tx_head = 0;
    for (int i = 0; i < I82574_NTX; i++) {
        s->txring[i].addr_lo = (uint32_t)(uintptr_t)s->txbuf[i];
        s->txring[i].addr_hi = 0;
        s->txring[i].status  = I82574_STATUS_DD;
    }

    i82574_ring(s, I82574_TD, s->txring, sizeof(s->txring));
    i82574_ring(s, I82574_RD, s->rxring, sizeof(s->rxring));

    // 82579: tx stalls if the reset value of MULR is lost
    i82574_wr(s, I82574_TCTL, (i82574_rd(s, I82574_TCTL) & ~I82574_TCTL_CT_COLD) |
              I82574_TCTL_EN | I82574_TCTL_PSP |
              I82574_TCTL_CT | I82574_TCTL_COLD);
    i82574_wr(s, I82574_RCTL, I82574_RCTL_EN | I82574_RCTL_BAM |
              I82574_RCTL_SECRC);

    i82574_wr(s, I82574_IMS, I82574_IRQ_RXT0 | I82574_IRQ_RXO |
              I82574_IRQ_RXDMT0);

    /* Go. */
    i82574_wr(s, I82574_RD + I82574_xDT, I82574_NRX - 1);
}

#endif
