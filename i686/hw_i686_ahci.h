/* AHCI SATA, polled, one port, one command slot, one PRDT entry.
   - no HBA reset: takes over the first port with a disk on it
   - 32 bit addresses, LBA48 DMA, no interrupts
   - spec: https://www.intel.com/content/dam/www/public/us/en/documents/technical-specifications/serial-ata-ahci-spec-rev1-3-1.pdf
   - ATA commands: ATA8-ACS, or Linux include/linux/ata.h */

#ifndef HW_I686_AHCI_H
#define HW_I686_AHCI_H

#include "hw_i686_pci.h"
#include "blockdev.h"
#include <stddef.h>
#include <string.h>

/* Matched by class 0106: x230 8086:1e03, qemu 8086:2922 */

/* HBA registers */
#define AHCI_CAP     0x00
#define AHCI_GHC     0x04
#define AHCI_PI      0x0C
#define AHCI_VS      0x10
#define AHCI_GHC_AE  0x80000000

/* Port registers, at 0x100 + 0x80 * port */
#define AHCI_PxCLB   0x00
#define AHCI_PxCLBU  0x04
#define AHCI_PxFB    0x08
#define AHCI_PxFBU   0x0C
#define AHCI_PxIS    0x10
#define AHCI_PxIE    0x14
#define AHCI_PxCMD   0x18
#define AHCI_PxTFD   0x20
#define AHCI_PxSIG   0x24
#define AHCI_PxSSTS  0x28
#define AHCI_PxSERR  0x30
#define AHCI_PxCI    0x38

#define AHCI_PxCMD_ST  0x0001
#define AHCI_PxCMD_FRE 0x0010
#define AHCI_PxCMD_FR  0x4000
#define AHCI_PxCMD_CR  0x8000
#define AHCI_PxIS_TFES 0x40000000
#define AHCI_TFD_ERR   0x01
#define AHCI_TFD_DRQ   0x08
#define AHCI_TFD_BSY   0x80
#define AHCI_SIG_ATA   0x00000101

#define ATA_CMD_READ_DMA_EXT   0x25
#define ATA_CMD_WRITE_DMA_EXT  0x35
#define ATA_CMD_FLUSH_EXT      0xEA
#define ATA_CMD_IDENTIFY       0xEC

#define AHCI_BUF  4096
#define AHCI_TIMEOUT 100000000

struct ahci_cmd_header {
    uint32_t flags;  // cfl, write, prdtl << 16
    uint32_t prdbc;
    uint32_t ctba;
    uint32_t ctbau;
    uint32_t reserved[4];
};

struct ahci_cmd_table {
    uint8_t cfis[64];
    uint8_t acmd[16];
    uint8_t reserved[48];
    struct {
        uint32_t dba;
        uint32_t dbau;
        uint32_t reserved;
        uint32_t dbc;  // bytes - 1
    } prdt[1];
};

struct ahci {
    volatile struct ahci_cmd_header cl[32] __attribute__((aligned(1024)));
    volatile uint8_t fis[256] __attribute__((aligned(256)));
    volatile struct ahci_cmd_table ct __attribute__((aligned(128)));
    uint8_t buf[AHCI_BUF] __attribute__((aligned(4096)));
    uint32_t abar;
    volatile uint8_t *port;
    uint32_t nb_sectors;  // from IDENTIFY, low 32 bits
    struct blockdev bd;   // set up when a disk is found
};

static inline uint32_t ahci_rd(struct ahci *s, uint32_t reg) {
    return *(volatile uint32_t *)(s->abar + reg);
}
static inline void ahci_wr(struct ahci *s, uint32_t reg, uint32_t val) {
    *(volatile uint32_t *)(s->abar + reg) = val;
}
static inline uint32_t ahci_prd(struct ahci *s, uint32_t reg) {
    return *(volatile uint32_t *)(s->port + reg);
}
static inline void ahci_pwr(struct ahci *s, uint32_t reg, uint32_t val) {
    *(volatile uint32_t *)(s->port + reg) = val;
}
/* Wait for (reg & mask) == val.  Returns -1 on timeout. */
static inline int ahci_wait(struct ahci *s, uint32_t reg, uint32_t mask, uint32_t val) {
    for (uint32_t i = 0; i < AHCI_TIMEOUT; i++) {
        if ((ahci_prd(s, reg) & mask) == val) return 0;
    }
    LOG("ahci timeout reg=%02x val=%08x\n", reg, ahci_prd(s, reg));
    return -1;
}

/* Data goes through s->buf, nb_bytes <= AHCI_BUF.  Returns 0 or -1. */
static inline int ahci_cmd(struct ahci *s, uint8_t cmd, uint32_t lba,
                           uint32_t nb_bytes, int write) {
    volatile struct ahci_cmd_table *ct = &s->ct;
    uint32_t count = nb_bytes / 512;
    for (int i = 0; i < 64; i++) ct->cfis[i] = 0;
    ct->cfis[0]  = 0x27;  // H2D register FIS
    ct->cfis[1]  = 0x80;  // command
    ct->cfis[2]  = cmd;
    ct->cfis[4]  = lba;
    ct->cfis[5]  = lba >> 8;
    ct->cfis[6]  = lba >> 16;
    ct->cfis[7]  = 0x40;  // LBA mode
    ct->cfis[8]  = lba >> 24;
    ct->cfis[12] = count;
    ct->cfis[13] = count >> 8;
    ct->prdt[0].dba  = (uint32_t)s->buf;
    ct->prdt[0].dbau = 0;
    ct->prdt[0].dbc  = nb_bytes - 1;
    s->cl[0].flags = 5 | (write ? 0x40 : 0) | ((nb_bytes ? 1 : 0) << 16);
    s->cl[0].prdbc = 0;
    s->cl[0].ctba  = (uint32_t)ct;
    s->cl[0].ctbau = 0;
    ahci_pwr(s, AHCI_PxIS, 0xFFFFFFFF);
    __asm__ volatile ("" ::: "memory");
    ahci_pwr(s, AHCI_PxCI, 1);
    for (uint32_t i = 0; i < AHCI_TIMEOUT; i++) {
        if (ahci_prd(s, AHCI_PxIS) & AHCI_PxIS_TFES) break;
        if (!(ahci_prd(s, AHCI_PxCI) & 1)) {
            __asm__ volatile ("" ::: "memory");
            return 0;
        }
    }
    LOG("ahci cmd %02x lba=%d failed is=%08x tfd=%08x\n",
        cmd, lba, ahci_prd(s, AHCI_PxIS), ahci_prd(s, AHCI_PxTFD));
    return -1;
}

static inline int ahci_read(struct ahci *s, uint32_t lba, uint32_t nb_bytes) {
    return ahci_cmd(s, ATA_CMD_READ_DMA_EXT, lba, nb_bytes, 0);
}
static inline int ahci_write(struct ahci *s, uint32_t lba, uint32_t nb_bytes) {
    return ahci_cmd(s, ATA_CMD_WRITE_DMA_EXT, lba, nb_bytes, 1);
}
static inline int ahci_flush(struct ahci *s) {
    return ahci_cmd(s, ATA_CMD_FLUSH_EXT, 0, 0, 0);
}

/* blockdev, copies through s->buf */
#define AHCI_BD(bd) ((struct ahci *)((uint8_t *)(bd) - offsetof(struct ahci, bd)))
static inline int ahci_bd_read(struct blockdev *bd, uint32_t lba, void *buf, uint32_t nb) {
    struct ahci *s = AHCI_BD(bd);
    uint8_t *dst = buf;
    while (nb) {
        uint32_t n = nb > AHCI_BUF / 512 ? AHCI_BUF / 512 : nb;
        if (ahci_read(s, lba, n * 512)) return -1;
        memcpy(dst, s->buf, n * 512);
        dst += n * 512; lba += n; nb -= n;
    }
    return 0;
}
static inline int ahci_bd_write(struct blockdev *bd, uint32_t lba, const void *buf, uint32_t nb) {
    struct ahci *s = AHCI_BD(bd);
    const uint8_t *src = buf;
    while (nb) {
        uint32_t n = nb > AHCI_BUF / 512 ? AHCI_BUF / 512 : nb;
        memcpy(s->buf, src, n * 512);
        if (ahci_write(s, lba, n * 512)) return -1;
        src += n * 512; lba += n; nb -= n;
    }
    return 0;
}
static inline int ahci_bd_flush(struct blockdev *bd) {
    return ahci_flush(AHCI_BD(bd));
}

static inline void ahci_init(struct ahci *s, const struct pci_function *f) {
    s->abar = pci_function_bar_mem(f, 0x24);  // BAR5
    // mem, bus master, INTx off
    pci_function_write16(f, 0x04, pci_function_read16(f, 0x04) | 0x406);
    ahci_wr(s, AHCI_GHC, ahci_rd(s, AHCI_GHC) | AHCI_GHC_AE);
    uint32_t pi = ahci_rd(s, AHCI_PI);
    LOG("ahci abar=%08x cap=%08x vs=%08x pi=%08x\n",
        s->abar, ahci_rd(s, AHCI_CAP), ahci_rd(s, AHCI_VS), pi);

    int p;
    for (p = 0; p < 32; p++) {
        if (!(pi & (1 << p))) continue;
        s->port = (volatile uint8_t *)(s->abar + 0x100 + 0x80 * p);
        uint32_t ssts = ahci_prd(s, AHCI_PxSSTS);
        uint32_t sig  = ahci_prd(s, AHCI_PxSIG);
        LOG("ahci port%d ssts=%08x sig=%08x\n", p, ssts, sig);
        if ((ssts & 0xF) == 3 && sig == AHCI_SIG_ATA) break;
    }
    if (p == 32) { s->port = 0; return; }

    // stop, move cl and fis to our buffers, start
    ahci_pwr(s, AHCI_PxCMD, ahci_prd(s, AHCI_PxCMD) & ~AHCI_PxCMD_ST);
    if (ahci_wait(s, AHCI_PxCMD, AHCI_PxCMD_CR, 0)) goto fail;
    ahci_pwr(s, AHCI_PxCMD, ahci_prd(s, AHCI_PxCMD) & ~AHCI_PxCMD_FRE);
    if (ahci_wait(s, AHCI_PxCMD, AHCI_PxCMD_FR, 0)) goto fail;
    for (int i = 0; i < 32; i++) s->cl[i].flags = 0;
    ahci_pwr(s, AHCI_PxCLB,  (uint32_t)s->cl);
    ahci_pwr(s, AHCI_PxCLBU, 0);
    ahci_pwr(s, AHCI_PxFB,   (uint32_t)s->fis);
    ahci_pwr(s, AHCI_PxFBU,  0);
    ahci_pwr(s, AHCI_PxIE,   0);
    ahci_pwr(s, AHCI_PxSERR, 0xFFFFFFFF);
    ahci_pwr(s, AHCI_PxIS,   0xFFFFFFFF);
    ahci_pwr(s, AHCI_PxCMD, ahci_prd(s, AHCI_PxCMD) | AHCI_PxCMD_FRE);
    if (ahci_wait(s, AHCI_PxTFD, AHCI_TFD_BSY | AHCI_TFD_DRQ, 0)) goto fail;
    ahci_pwr(s, AHCI_PxCMD, ahci_prd(s, AHCI_PxCMD) | AHCI_PxCMD_ST);

    if (ahci_cmd(s, ATA_CMD_IDENTIFY, 0, 512, 0)) goto fail;
    uint16_t *id = (uint16_t *)s->buf;
    s->nb_sectors = id[100] | (id[101] << 16);
    char model[41];
    for (int i = 0; i < 20; i++) {  // byte swapped
        model[2*i]   = id[27+i] >> 8;
        model[2*i+1] = id[27+i];
    }
    model[40] = 0;
    LOG("ahci port%d sectors=%d model=%s\n", p, s->nb_sectors, model);
    s->bd = (struct blockdev){
        .read  = ahci_bd_read,
        .write = ahci_bd_write,
        .flush = ahci_bd_flush,
        .nb_sectors = s->nb_sectors,
    };
    return;
  fail:
    s->port = 0;
}

#endif
