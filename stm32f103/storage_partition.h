#ifndef PARTITION_H
#define PARTITION_H

struct hw_sdio_card;
struct device;
union device_impl {
    struct hw_sdio_card *sd;
};
#define DEVICE_TYPE_SD   1
struct device {
    /* Physical access. */
    uint32_t blocks_per_cluster;
    uint32_t type;
    union device_impl impl;
};
struct partition {
    /* Logical */
    uint32_t start_cluster;
    uint32_t nb_clusters;
    /* Physical device (static). */
    struct device *device;
};

#endif//PARTITION_H
