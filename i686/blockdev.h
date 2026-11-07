/* Block device interface, 512 byte sectors.
   - drivers embed a struct blockdev and fill in the methods
   - partition is a blockdev on top of another one
   - no ABI guarantees: users are built against the kernel headers */

#ifndef BLOCKDEV_H
#define BLOCKDEV_H

#include <stdint.h>

#define BLOCKDEV_SECTOR 512

struct blockdev;
typedef int (*blockdev_read_fn)(struct blockdev *, uint32_t lba, void *buf, uint32_t nb);
typedef int (*blockdev_write_fn)(struct blockdev *, uint32_t lba, const void *buf, uint32_t nb);
typedef int (*blockdev_flush_fn)(struct blockdev *);

/* Methods return 0 or -1, nb counts sectors. */
struct blockdev {
    blockdev_read_fn  read;
    blockdev_write_fn write;
    blockdev_flush_fn flush;
    uint32_t nb_sectors;
};

/* MBR partition */
struct partition {
    struct blockdev bd;  // first, see partition_*
    struct blockdev *parent;
    uint32_t lba0;
};
static inline int partition_read(struct blockdev *bd, uint32_t lba, void *buf, uint32_t nb) {
    struct partition *p = (struct partition *)bd;
    if (lba + nb > bd->nb_sectors) return -1;
    return p->parent->read(p->parent, p->lba0 + lba, buf, nb);
}
static inline int partition_write(struct blockdev *bd, uint32_t lba, const void *buf, uint32_t nb) {
    struct partition *p = (struct partition *)bd;
    if (lba + nb > bd->nb_sectors) return -1;
    return p->parent->write(p->parent, p->lba0 + lba, buf, nb);
}
static inline int partition_flush(struct blockdev *bd) {
    struct partition *p = (struct partition *)bd;
    return p->parent->flush(p->parent);
}
/* First primary partition of the given type.  Returns 0 or -1. */
static inline int partition_find(struct partition *p, struct blockdev *parent, uint8_t type) {
    uint8_t mbr[BLOCKDEV_SECTOR];
    if (parent->read(parent, 0, mbr, 1)) return -1;
    if (mbr[510] != 0x55 || mbr[511] != 0xaa) return -1;
    for (int i = 0; i < 4; i++) {
        const uint8_t *e = mbr + 0x1be + 16 * i;
        if (e[4] != type) continue;
        p->bd = (struct blockdev){
            .read  = partition_read,
            .write = partition_write,
            .flush = partition_flush,
            .nb_sectors = e[12] | (e[13] << 8) | (e[14] << 16) | ((uint32_t)e[15] << 24),
        };
        p->parent = parent;
        p->lba0 = e[8] | (e[9] << 8) | (e[10] << 16) | ((uint32_t)e[11] << 24);
        return 0;
    }
    return -1;
}

#endif
