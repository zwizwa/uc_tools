/* ATA PIO on a legacy IDE channel, polled, LBA28, see blockdev.h
   - for CF on Soekris (Geode IDE) and qemu's piix IDE
   - device interrupt off (nIEN), runs from the main loop
   - FLUSH CACHE aborted means no write cache: treated as success
   - https://wiki.osdev.org/ATA_PIO_Mode, ATA8-ACS for the commands */

#ifndef HW_I686_ATA_PIO_H
#define HW_I686_ATA_PIO_H

#include "hw_i686_io.h"
#include "blockdev.h"
#include <stddef.h>

/* Primary channel */
#define ATA_PRIMARY_IO   0x1F0
#define ATA_PRIMARY_CTL  0x3F6

/* io + */
#define ATA_DATA     0
#define ATA_ERR      1
#define ATA_COUNT    2
#define ATA_LBA_LO   3
#define ATA_LBA_MID  4
#define ATA_LBA_HI   5
#define ATA_DRIVE    6
#define ATA_STATUS   7  // read
#define ATA_COMMAND  7  // write
/* ctl + 0 */
#define ATA_CTL_NIEN 0x02

#define ATA_ST_ERR   0x01
#define ATA_ST_DRQ   0x08
#define ATA_ST_DF    0x20
#define ATA_ST_BSY   0x80
#define ATA_ERR_ABRT 0x04

#define ATA_PIO_READ     0x20
#define ATA_PIO_WRITE    0x30
#define ATA_PIO_FLUSH    0xE7
#define ATA_PIO_IDENTIFY 0xEC

#define ATA_TIMEOUT 10000000

struct ata_pio {
    uint16_t io, ctl;
    uint8_t slave;
    uint8_t no_flush;     // FLUSH CACHE aborted once
    uint32_t nb_sectors;  // 0: no disk
    struct blockdev bd;
};

static inline uint8_t ata_status(struct ata_pio *s) {
    return inb(s->io + ATA_STATUS);
}
/* 400ns after drive select: 4 reads of alternate status. */
static inline void ata_delay(struct ata_pio *s) {
    for (int i = 0; i < 4; i++) inb(s->ctl);
}
/* Wait for BSY clear, and DRQ if drq.  Returns -1 on error or timeout. */
static inline int ata_wait(struct ata_pio *s, int drq) {
    for (uint32_t i = 0; i < ATA_TIMEOUT; i++) {
        uint8_t st = ata_status(s);
        if (st & ATA_ST_BSY) continue;
        if (st & (ATA_ST_ERR | ATA_ST_DF)) return -1;
        if (!drq || (st & ATA_ST_DRQ)) return 0;
    }
    return -1;
}
static inline void ata_cmd(struct ata_pio *s, uint8_t cmd, uint32_t lba, uint8_t count) {
    outb(s->io + ATA_DRIVE, 0xE0 | (s->slave << 4) | ((lba >> 24) & 0x0F));
    ata_delay(s);
    outb(s->io + ATA_COUNT,   count);
    outb(s->io + ATA_LBA_LO,  lba);
    outb(s->io + ATA_LBA_MID, lba >> 8);
    outb(s->io + ATA_LBA_HI,  lba >> 16);
    outb(s->io + ATA_COMMAND, cmd);
}

#define ATA_BD(bd) ((struct ata_pio *)((uint8_t *)(bd) - offsetof(struct ata_pio, bd)))
static inline int ata_pio_read(struct blockdev *bd, uint32_t lba, void *buf, uint32_t nb) {
    struct ata_pio *s = ATA_BD(bd);
    uint16_t *w = buf;
    while (nb) {
        uint32_t n = nb > 256 ? 256 : nb;  // count 0 means 256
        if (ata_wait(s, 0)) return -1;
        ata_cmd(s, ATA_PIO_READ, lba, n);
        for (uint32_t i = 0; i < n; i++) {
            if (ata_wait(s, 1)) return -1;
            for (int j = 0; j < 256; j++) *w++ = inw(s->io + ATA_DATA);
        }
        lba += n; nb -= n;
    }
    return 0;
}
static inline int ata_pio_write(struct blockdev *bd, uint32_t lba, const void *buf, uint32_t nb) {
    struct ata_pio *s = ATA_BD(bd);
    const uint16_t *w = buf;
    while (nb) {
        uint32_t n = nb > 256 ? 256 : nb;
        if (ata_wait(s, 0)) return -1;
        ata_cmd(s, ATA_PIO_WRITE, lba, n);
        for (uint32_t i = 0; i < n; i++) {
            if (ata_wait(s, 1)) return -1;
            for (int j = 0; j < 256; j++) outw(s->io + ATA_DATA, *w++);
        }
        if (ata_wait(s, 0)) return -1;
        lba += n; nb -= n;
    }
    return 0;
}
static inline int ata_pio_flush(struct blockdev *bd) {
    struct ata_pio *s = ATA_BD(bd);
    if (s->no_flush) return 0;
    if (ata_wait(s, 0)) return -1;
    ata_cmd(s, ATA_PIO_FLUSH, 0, 0);
    if (!ata_wait(s, 0)) return 0;
    if (inb(s->io + ATA_ERR) & ATA_ERR_ABRT) {
        LOG("ata no FLUSH CACHE, no write cache\n");
        s->no_flush = 1;
        return 0;
    }
    return -1;
}

/* Returns 0 when an ATA disk answers IDENTIFY. */
static inline int ata_pio_init(struct ata_pio *s, uint16_t io, uint16_t ctl, uint8_t slave) {
    s->io = io;
    s->ctl = ctl;
    s->slave = slave;
    s->nb_sectors = 0;
    outb(ctl, ATA_CTL_NIEN);
    outb(io + ATA_DRIVE, 0xA0 | (slave << 4));
    ata_delay(s);
    uint8_t st = ata_status(s);
    if (st == 0xFF || st == 0) return -1;  // floating bus, no drive
    outb(io + ATA_COUNT, 0);
    outb(io + ATA_LBA_LO, 0);
    outb(io + ATA_LBA_MID, 0);
    outb(io + ATA_LBA_HI, 0);
    outb(io + ATA_COMMAND, ATA_PIO_IDENTIFY);
    if (!ata_status(s)) return -1;
    for (uint32_t i = 0; i < ATA_TIMEOUT && (ata_status(s) & ATA_ST_BSY); i++);
    // ATAPI and SATA set the signature in LBA mid/hi
    if (inb(io + ATA_LBA_MID) || inb(io + ATA_LBA_HI)) return -1;
    if (ata_wait(s, 1)) return -1;
    uint16_t id[256];
    for (int i = 0; i < 256; i++) id[i] = inw(io + ATA_DATA);
    s->nb_sectors = id[60] | ((uint32_t)id[61] << 16);
    char model[41];
    for (int i = 0; i < 20; i++) {  // byte swapped
        model[2*i]   = id[27+i] >> 8;
        model[2*i+1] = id[27+i];
    }
    model[40] = 0;
    LOG("ata %03x%s sectors=%d model=%s\n", io, slave ? " slave" : "",
        s->nb_sectors, model);
    s->bd = (struct blockdev){
        .read  = ata_pio_read,
        .write = ata_pio_write,
        .flush = ata_pio_flush,
        .nb_sectors = s->nb_sectors,
    };
    return 0;
}

#endif
