#ifndef SM_SDIO_H
#define SM_SDIO_H

/* SD data transfer state machine. */

#include "sm.h"
#include "base.h"

#include "storage_impl.h"



/* Composite state  machine for multiple block write.
   The sub-machine state can be shared as calls are mutually exclusive. */

/* Sub machine for command sending */
struct sm_sdio_cmd {
    /* State */
    void *next;

    /* Parameters */
    struct transfer_context *ctx;
    uint32_t cmd;
    uint32_t arg;
};

/* Sub machine for data sending
   Buffers are represented in the abstract to avoid
   large contiguous chunks of memory. */
struct sm_sdio_data;




struct sm_sdio_data {
    /* State */
    void *next;

    uint32_t block;

    /* Parameters */
    struct transfer_context *ctx;
    uint32_t nb_blocks;
    uint32_t logsize;
    uint32_t *abort;
};
struct sm_sdio_write_cluster {
    /* State */
    void *next;
    union {
        struct sm_sdio_cmd cmd;
        struct sm_sdio_data data;
    } sub;
    /* Params */
    struct transfer_context *ctx;
    uint32_t nb_blocks;
    uint32_t logsize;
    uint32_t lba;
};

struct sm_sdio_erase {
    void *next;
    struct transfer_context ctx;

    struct cb_void *done;
    uint32_t cur;
    uint32_t endx;
    uint32_t cluster;
    union {
        struct sm_sdio_cmd cmd;
    } sub;
};

void sm_sdio_write_cluster_init(struct sm_sdio_write_cluster *sm,
                                struct transfer_context *ctx,
                                uint32_t nb_blocks, uint32_t logsize,
                                uint32_t lba);
uint32_t sm_sdio_write_cluster_tick(struct sm_sdio_write_cluster *sm);

void sm_sdio_data_init(struct sm_sdio_data *sm,
                       struct transfer_context *ctx,
                       uint32_t nb_blocks, uint32_t logsize);
uint32_t sm_sdio_data_tick(struct sm_sdio_data *sm);

void sm_sdio_cmd_init(struct sm_sdio_cmd *sm,
                      struct transfer_context *sd,
                      uint32_t cmd, uint32_t arg);
uint32_t sm_sdio_cmd_tick(struct sm_sdio_cmd *sm);

/* Shorthand macros.
                                        fn prefix      state          _init() arguments
--------------------------------------------------------------------------------------- */
#define SM_CMD(sm,ctx,...)  SM_CALL(sm, sm_sdio_cmd,   &sm->sub.cmd,  ctx,  __VA_ARGS__)
#define SM_DATA(sm,ctx,...) SM_CALL(sm, sm_sdio_data,  &sm->sub.data, ctx,  __VA_ARGS__)



void sm_sdio_erase_init(struct sm_sdio_erase *sm,
                        struct device *device,
                        struct cb_void *done,
                        uint32_t lba_start, uint32_t lba_endx, uint32_t lba_cluster);
uint32_t sm_sdio_erase_tick(struct sm_sdio_erase *sm);


/* SDIO Chunker to 512-byte blocks. */
struct sdio_block_stream {
    struct obj_stream out_stream;
    struct obj_stream *in_stream;
    uint32_t out_count;
    uint8_t buf[BLOCK_SIZE];
};
void sdio_block_stream_init(struct sdio_block_stream *x,
                            struct obj_stream *in);


#endif
