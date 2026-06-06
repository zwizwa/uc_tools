#ifndef HW_I686_ATA_PIO_H
#define  HW_I686_ATA_PIO_H

// Output from claude, needs review.
// https://claude.ai/chat/a46f6d44-042f-42b8-a63f-488954359364


#include <stdint.h>

struct i686_ata_pio {
};


/* Provided by your kernel */
extern void     outb(uint16_t port, uint8_t  val);
extern uint8_t  inb(uint16_t port);
extern uint16_t inw(uint16_t port);

/* Primary ATA channel I/O ports */
#define ATA_DATA      0x1F0
#define ATA_ERR       0x1F1
#define ATA_SECCOUNT  0x1F2
#define ATA_LBA_LO    0x1F3
#define ATA_LBA_MID   0x1F4
#define ATA_LBA_HI    0x1F5
#define ATA_DRIVE     0x1F6
#define ATA_STATUS    0x1F7  /* read */
#define ATA_COMMAND   0x1F7  /* write */

#define ST_BSY  0x80
#define ST_DRQ  0x08
#define ST_ERR  0x01

#define CMD_READ_PIO  0x20

/* Read one 512-byte sector at `lba` (LBA28) from master drive into buf. */
/* Returns 0 on success, -1 on error. */
int ata_read_sector(uint32_t lba, void *buf) {
    /* Wait for not-busy */
    while (inb(ATA_STATUS) & ST_BSY)
        ;

    /* Drive/head: master (0xE0), top 4 bits of LBA, LBA mode */
    outb(ATA_DRIVE,    0xE0 | ((lba >> 24) & 0x0F));
    outb(ATA_SECCOUNT, 1);
    outb(ATA_LBA_LO,   (uint8_t)(lba));
    outb(ATA_LBA_MID,  (uint8_t)(lba >> 8));
    outb(ATA_LBA_HI,   (uint8_t)(lba >> 16));
    outb(ATA_COMMAND,  CMD_READ_PIO);

    /* Wait until BSY clears and DRQ sets (or error) */
    uint8_t st;
    do {
        st = inb(ATA_STATUS);
        if (st & ST_ERR) return -1;
    } while ((st & ST_BSY) || !(st & ST_DRQ));

    /* Transfer 256 words = 512 bytes */
    uint16_t *p = (uint16_t *)buf;
    for (int i = 0; i < 256; i++)
        p[i] = inw(ATA_DATA);

    return 0;
}
