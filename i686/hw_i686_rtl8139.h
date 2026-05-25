/* Support for RTL 8139 100M PCI network card. */

#ifndef HW_RTL8139
#define HW_RTL8139



#include "hw_i686_pci.h"
#include "macros.h"
#include <string.h>

/* If logging is not defined in any of the headers that we include or
   in the compilation unit that includes us, we define it as a
   no-op. */
#ifndef LOG
#define LOG(...)
#endif


/* RTL8139 */

#define rtl8139_vendor  0x10ec
#define rtl8139_device  0x8139

#define RTL_IDR0        0x00   // MAC address (6 bytes)
#define RTL_MAR0        0x08   // Multicast filter
#define RTL_TSD0        0x10   // Transmit Status (4 descriptors: 0x10-0x1C)
#define RTL_TSAD0       0x20   // Transmit Start Address (4 descriptors: 0x20-0x2C)
#define RTL_RBSTART     0x30   // RX buffer start address
#define RTL_CMD         0x37   // Command register
#define RTL_CAPR        0x38   // Current address of packet read
#define RTL_CBR         0x3A
#define RTL_IMR         0x3C   // Interrupt mask
#define RTL_ISR         0x3E   // Interrupt status
#define RTL_RCR         0x44   // RX config
#define RTL_MPC         0x4C   // Missed Packet Counter
#define RTL_9346CR      0x50   // EEPROM Command Register
#define RTL_CONFIG1     0x52   // Config 1
#define RTL_MSR         0x58   // Media Status Register

// CMD bits
#define CMD_RST         0x10
#define CMD_RE          0x08   // RX enable
#define CMD_TE          0x04   // TX enable

// ISR/IMR bits
#define INT_ROK         (1<<0) // RX OK
#define INT_RER         (1<<1) // RX error
#define INT_TOK         (1<<2) // TX OK
#define INT_TER         (1<<3) // TX error
#define INT_RXOVW       (1<<4) // RX buffer overflow
#define INT_PUN         (1<<5) // Packet Underrun or Link Change
#define INT_FOVW        (1<<6) // RX fifo overflow

// RCR bits
#define RCR_AAP         (1<<0) // Accept all packets (promiscuous)
#define RCR_APM         (1<<1) // Accept physical match
#define RCR_AM          (1<<2) // Accept multicast
#define RCR_AB          (1<<3) // Accept broadcast
#define RCR_WRAP        (1<<7) // Ring wrap enable — keep this SET
#define RCR_MXDMA_UNLIM (7<<8) // Max DMA burst size unlimited


/* TSD bits */
#define TSD_OWN         (1 << 13)   /* DMA done when set by NIC */
#define TSD_TUN         (1 << 14)   /* Tx FIFO underrun */
#define TSD_TOK         (1 << 15)   /* Tx OK */
#define TSD_SIZE_MASK   0x1FFF      /* bits 0-12 = packet size */


#define RCR_RXFTH_NONE  (7<<13) // No receive threshold


// RCR bits 12:11 — RBLEN
//  00  8K
//  01  16K
//  10  32K
//  11  64K

// There seems to be no reason to pick anything less than the maximum.
// The 8K is not enough in practice, but can be used to debug RX reset
// with flooded ethernet.
#define RCR_RBLEN 3
#define RCR_RBLEN_SHIFT 11
#define RTL8139_RX_BUF_LEN (1<<(RCR_RBLEN+13))

#define RTL8139_TX_BUF_SIZE 2048   /* per-descriptor buffer; max packet is 1792 */


struct rtl8139_tx {
    uint8_t buf[RTL8139_TX_BUF_SIZE]  __attribute__((aligned(4)));
};
struct rtl8139 {
    uint8_t  rx_buf[RTL8139_RX_BUF_LEN + 2048]  __attribute__((aligned(4)));
    uint32_t rx_offset;
    uint32_t rx_isr_count;
    uint32_t tx_isr_count;
    uint32_t iobase;
    uint8_t  irq;
    uint8_t  mac[6];
    struct rtl8139_tx tx[4];
    uint8_t  tx_index:2;

    void (*rx)(void *, const uint8_t *, uint32_t);
    void *ctx;

};

static inline void rtl8139_rx_poll(struct rtl8139 *s) {
    // CMD bit 0 = RX buffer empty
    uint32_t mask = (RTL8139_RX_BUF_LEN - 1) & (~3);
    while (!(inb(s->iobase + RTL_CMD) & 0x01)) {
        uint8_t *entry = s->rx_buf + s->rx_offset;
        //uint32_t o0 = s->rx_offset;

        uint16_t rx_status = *(uint16_t *)(entry + 0);
        uint16_t rx_len    = *(uint16_t *)(entry + 2); // includes 4-byte CRC

        if (!(rx_status & 0x0001)) {
            // ROK not set — bad packet or not yet ready
            break;
        }

        uint8_t  *pkt     = entry + 4;
        uint16_t  pkt_len = rx_len - 4; // strip CRC
        (void)pkt;
        (void)pkt_len;

        // Hand packet up to your network stack
        s->rx(s->ctx, pkt, pkt_len);
        //LOG("pkt len=%d\n", pkt_len);
        //log_hex(pkt, pkt_len);

        // Advance read pointer — DWORD aligned, wrapped.
        s->rx_offset = (s->rx_offset + 4 + rx_len + 3) & mask;
        //uint32_t o1 = s->rx_offset;

        //LOG("offset %04x->%04x\n", o0, o1);
        //volatile uint8_t *top_right = (void*)(0xB8000 + 2*79);
        //(*top_right) = progress[(s->count++)&7];
        //spinner(2,s->count++);

        // Tell card we've consumed up to here.
        // CAPR is written as rx_offset-16, a hardware quirk
        outw(s->iobase + RTL_CAPR, (s->rx_offset - 16) & mask);
    }
}

/* Returns 0 on success, -1 if no descriptor is free or packet too big. */
int rtl8139_transmit(struct rtl8139 *s, const void *data, uint16_t len) {

    uint32_t desc = s->tx_index;
    // LOG("desc=%d\n", desc);
    uint16_t tsd_port = s->iobase + RTL_TSD0 + desc * 4;

    if (len > sizeof(s->tx[desc].buf)) return -1;


    /* If OWN is 0, the card is still working on this descriptor. */
    uint32_t status = inl(tsd_port);
    if (!(status & TSD_OWN) && status != 0) {
        /* Descriptor still owned by NIC. Caller should retry or queue. */
        return -1;
    }

    /* Copy packet into the descriptor's buffer. */
    uint8_t *buf = &s->tx[desc].buf[0];
    memcpy(buf, data, len);

    if (len < 60) {
        // FIXME zero the buffer
        // uint16_t pad = 60 - len;
        // memset(
        len = 60;
    }
    /* Kick the NIC: writing the size (with OWN=0, ER thresh default)
       starts the DMA. The card sets OWN=1 when it's done. */
    outl(tsd_port, (uint32_t)(len & TSD_SIZE_MASK));

    s->tx_index++;
    return 0;
}



static inline void rtl8139_reset(struct rtl8139 *s) {

    // Set up transmit buffers
    for (int i = 0; i < ARRAY_SIZE(s->tx); i++) {
        outl(s->iobase + RTL_TSAD0 + i * 4, (uint32_t)&s->tx[i].buf[0]);
    }
    s->tx_index = 0;

    // Enable RX + TX
    outb(s->iobase + RTL_CMD, CMD_RE | CMD_TE);

    // Set RX config
    uint32_t rcr_val =
        //RCR_AAP | // all packets
        RCR_AB | RCR_APM | RCR_AM |
        RCR_WRAP | (RCR_RBLEN << RCR_RBLEN_SHIFT) | RCR_MXDMA_UNLIM |
        RCR_RXFTH_NONE;
    outl(s->iobase + RTL_RCR, rcr_val);

    // Offfset of next packet
    s->rx_offset = 0;

}


static inline void rtl8139_isr_inner(struct rtl8139 *s) {
    uint16_t status = inw(s->iobase + RTL_ISR);
    // Acknowledge all raised interrupts
    outw(s->iobase + RTL_ISR, status);

    //uint16_t status_ = inw(s->iobase + RTL_ISR);
    //LOG("rtl8139 interrupt %02x %02x\n", status, status_);
    if (status & INT_ROK) {
        spinner(2, s->rx_isr_count++);
        rtl8139_rx_poll(s);
    }

    if (status & INT_TOK) {
        // LOG("TOK\n");
        spinner(3, s->tx_isr_count++);
        outw(s->iobase + RTL_ISR, INT_TOK | INT_TER);
        /* Optionally walk the four TSD registers and check for errors:
           TUN (underrun), TABT (aborted), OWC (out of window collision), etc.
           For a minimal driver, just acking is enough.
           https://claude.ai/chat/7c27ad13-8f34-4f49-9418-a6a3ffd3254d
        */
    }

    if (status & (INT_RER | INT_RXOVW)) {
        LOG("rtl8139 error %04x\n", status);
        // Disable receiver
        outb(s->iobase + RTL_CMD, CMD_TE);
        // Re-initialize
        rtl8139_reset(s);
    }


    // Caller will EOI to PIC
}

static inline void rtl8139_status(struct rtl8139 *s) {
    LOG("CBR=%04x CAPR=%04x MPC=%d ISR=%d CMD=%02x MSR=%02x RCR=%08x\n"
        ,inw(s->iobase + RTL_CBR)
        ,inw(s->iobase + RTL_CAPR)
        ,inl(s->iobase + RTL_MPC)
        ,inw(s->iobase + RTL_ISR)
        ,inb(s->iobase + RTL_CMD)
        ,inb(s->iobase + RTL_MSR)
        ,inl(s->iobase + RTL_RCR)
        );
}

static inline void rtl8139_rx_ignore(void *s, const uint8_t *data, uint32_t len) {
}

static inline void rtl8139_init(struct rtl8139 *s,
                                const struct pci_function *f) {
    s->iobase = pci_function_read32(f, PCI_CFG_BAR0) & ~3; // BAR0, strip I/O bit
    s->irq    = pci_function_read32(f, PCI_CFG_IRQ) & 0xFF;

    s->rx = rtl8139_rx_ignore;

    for (int i = 0; i < 6; i++) {
        s->mac[i] = inb(s->iobase + RTL_IDR0 + i);
    }

    // LOG("RTL8139_RX_BUF_LEN=%d\n", RTL8139_RX_BUF_LEN);


    // Enable PCI Bus Mastering + I/O Space
    uint16_t cmd = pci_function_read16(f, 0x04);
    pci_function_write16(f, 0x04, cmd | 0x5);
    //uint16_t cmd_ = pci_function_read16(f, 0x04);
    // LOG("cmd=%04x->cmd=%04x\n", cmd, cmd_);

    /* Unlock config registers (some real hardware needs this) */
    // outb(s->iobase + 0x50, 0xC0); /* 9346CR: config write enable */

    // Power on
    outb(s->iobase + RTL_CONFIG1, 0x00);

    // Software reset — wait for RST bit to clear
    outb(s->iobase + RTL_CMD, CMD_RST);
    while (inb(s->iobase + RTL_CMD) & CMD_RST);

    // Give card the physical RX buffer address
    outl(s->iobase + RTL_RBSTART, (uint32_t)s->rx_buf);

    // Also called from isr
    rtl8139_reset(s);

    // Accept all multicast
    outl(s->iobase + RTL_MAR0,   0xFFFFFFFF);
    outl(s->iobase + RTL_MAR0+4, 0xFFFFFFFF);

    // Unmask interrupts
    outw(s->iobase + RTL_IMR,
         INT_ROK | INT_RER | INT_RXOVW |
         INT_TOK | INT_TER);

    /* Lock config registers */
    //outb(s->iobase + 0x50, 0x00);          /* 9346CR: normal mode */


    // uint8_t msr = inb(s->iobase + 0x58);
    // LOG("msr = %02x\n", msr);

    LOG("rtl8139 io=%04x irq=%d mac=%02x:%02x:%02x:%02x:%02x:%02x\n",
        s->iobase,
        s->irq,
        s->mac[0],
        s->mac[1],
        s->mac[2],
        s->mac[3],
        s->mac[4],
        s->mac[5]);

}



#endif
