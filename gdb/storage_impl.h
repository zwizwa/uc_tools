#ifndef STORAGE_IMPL_H
#define STORAGE_IMPL_H

// Partitions are always cluster-aligned.
// Clusters correspond to Flash erase units.
// Blocks are a logical unit used by firmware.

#include "storage_partition.h"
#include "hw_sdio.h"

/* This structure captures context of a transfer from a block_stream
 * to a concrete device. */
typedef void (*transfer_context_finalize_fn)(void *finalize_ctx, void *block_data);
struct transfer_context;
struct transfer_context {
    struct device *device;
    struct obj_stream *block_stream;
    uint32_t abort;
    transfer_context_finalize_fn finalize;
    void *finalize_ctx;
    volatile uint32_t *frame_count;
};

static inline uint32_t device_nb_blocks(struct device *d) {
    switch(d->type) {
    case DEVICE_TYPE_SD:
        return d->impl.sd->nb_units / SD_UNITS_PER_BLOCK;
    default:
        return 0;
    }
}


/* Disks are partitioned in two:

   - Header partition.  The first cluster is reserved for later
     metadata.  This is mandatory on SD, so we do the same on NAND.

   - Data partition: everything after the first cluster, rounded down
     to a multiple of clusters.

   Cluster size is determined by the device's erase unit.
*/


static inline void data_partition(struct partition *part,
                                  struct device *device,
                                  uint32_t start_cluster) {
    memset(part,0,sizeof(*part));
    part->device = device;
    part->start_cluster = start_cluster;
    part->nb_clusters =
        (device_nb_blocks(device) / device->blocks_per_cluster)
        - start_cluster;
}

/* Maximum number of storage devices supported (for static allocation) */
#define MAX_NB_STORE 3

#endif
