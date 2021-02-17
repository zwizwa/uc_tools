#include "base.h"
#include "sm_sdio.h"
#include <stdio.h>

#define DEBUG 0

static inline void trace_time(uint32_t tag) { }
#define TRACE_TIME(tag) trace_time(tag)

/* Use in conjunction with SM_WAIT() */
uint32_t sm_sdio_wait_sta(struct hw_sdio_card *sd, uint32_t expected, uint32_t errors) {
    uint32_t sta = SDIO_STA;
    if (0 && DEBUG) printf("sm_sdio_wait_sta(%08x,%08x) -> %08x\n",
                      (unsigned int)expected,
                      (unsigned int)errors,
                      (unsigned int)sta);
    if (sta & errors) {
        hw_sdio_sta_record_errors(sd, sta);
        return sta;
    }
    if (sta & expected) {
        SDIO_ICR = sta; // acknowledge
        if (sta & SDIO_STA_CMDREND) { sd->stats.cmdrend++; }
        if (sta & SDIO_STA_DBCKEND) { sd->stats.dbckend++; }
        return SM_HALTED;
    }
    /* Nothing, retry. */
    return SM_WAITING;
}

/* Keep polling until something happens.
   Continue on expected flags, but abort on error. */
#define SM_WAIT_STA(sm, expected)                                       \
    SM_WAIT_TICK_HALT(                                                  \
        sm,                                                             \
        sm_sdio_wait_sta(                                               \
            sm->ctx->device->impl.sd,                                   \
            expected, HW_SDIO_STATUS_ERROR_MASK))

/* Wait for SD card to be ready, or pass this synchronization point
   through the spillover mechanism if one is provided. */
#define SM_WAIT_READY(sm)                   \
    SM_WAIT(sm, hw_sdio_ready())

/* Sub-machine for command transfers. */
void sm_sdio_cmd_init(struct sm_sdio_cmd *sm,
                      struct transfer_context *ctx,
                      uint32_t cmd, uint32_t arg) {
    memset(sm,0,sizeof(*sm));
    sm->ctx = ctx;
    sm->cmd = cmd;
    sm->arg = arg;
}
uint32_t sm_sdio_cmd_tick(struct sm_sdio_cmd *sm) {
    SM_RESUME(sm);
    SM_WAIT_READY(sm);                                TRACE_TIME(1);
    hw_sdio_cmd(sm->cmd, sm->arg);
    SM_WAIT_STA(sm, SDIO_STA_CMDREND);                TRACE_TIME(2);
    SM_WAIT_READY(sm);                                TRACE_TIME(3);
  halt:
    SM_HALT(sm);
}


/* Sub-machine for data transfers. */
void sm_sdio_data_init(struct sm_sdio_data *sm,
                       struct transfer_context *ctx,
                       uint32_t nb_blocks, uint32_t logsize) {
    memset(sm,0,sizeof(*sm));
    sm->ctx = ctx;
    sm->nb_blocks = nb_blocks;
    sm->logsize = logsize;
}
uint32_t sm_sdio_data_tick(struct sm_sdio_data *sm) {
    SM_RESUME(sm);
    SM_WAIT_READY(sm);                                TRACE_TIME(4);
    for(sm->block = 0;
        sm->block < sm->nb_blocks;
        sm->block++) {
        void *buf;
        SM_WAIT(sm,
                (buf = obj_stream_cur((sm->ctx->block_stream)))
                || sm->ctx->abort);                   TRACE_TIME(5);
        if (sm->ctx->abort) {
            //LOG_EMU("sd_sdio_data_tick: abort\n");
            infof("abort: not writing cluster-relative block %d\n", sm->block);
            break;
        }
        if (0 == (sm->block % SD_UNITS_PER_BLOCK)) {
            /* We have a couple of SD card write units (512 bytes) per
               logical data block (2K).  The header is in the first
               one only. */
            // FIXME: messy -- move this into sdio_block_stream
            sm->ctx->finalize(sm->ctx->finalize_ctx, buf);
        }

        hw_sdio_send_unit(buf, sm->logsize);
        SM_WAIT_STA(sm, SDIO_STA_DBCKEND);            TRACE_TIME(6);
        /* Ack before waiting: wait might initiate spillover. */
        obj_stream_ack(sm->ctx->block_stream);
        SM_WAIT_READY(sm);                            TRACE_TIME(7);
    }
  halt:
    SM_HALT(sm);
}





/* Sub-machine for multiple block transfer. */
void sm_sdio_write_cluster_init(struct sm_sdio_write_cluster *sm,
                                struct transfer_context *ctx,
                                uint32_t nb_blocks, uint32_t logsize,
                                uint32_t lba) {
    memset(sm,0,sizeof(*sm));
    sm->ctx = ctx;
    sm->nb_blocks = nb_blocks;
    sm->logsize = logsize;
    sm->lba = lba;
}


uint32_t sm_sdio_write_cluster_tick(struct sm_sdio_write_cluster *sm) {
    SM_RESUME(sm);

    if (!hw_sdio_card_ccs()) {
        SM_CMD(sm, sm->ctx, SDIO_CMD_SET_BLOCKLEN, 1 << sm->logsize);
    }
    SM_CMD (sm, sm->ctx, SDIO_CMD_APP_CMD, sm->ctx->device->impl.sd->rca << 16);
    SM_CMD (sm, sm->ctx, SDIO_ACMD_SET_WR_BLK_ERASE_COUNT, sm->nb_blocks);
    SM_CMD (sm, sm->ctx, SDIO_CMD_WRITE_MULTIPLE_BLOCK, hw_sdio_card_lba_to_addr(sm->lba));
    SM_DATA(sm, sm->ctx, sm->nb_blocks, sm->logsize);
    SM_CMD (sm, sm->ctx, SDIO_CMD_STOP_TRANSMISSION, 0);

  halt:
    SM_HALT(sm);
}


void sm_sdio_erase_init(struct sm_sdio_erase *sm,
                        struct device *device,
                        struct cb_void *done,
                        uint32_t lba_start, uint32_t lba_endx, uint32_t lba_cluster) {
    sm->ctx.device = device;
    sm->next = 0;
    sm->done = done;
    sm->cur = lba_start;
    sm->endx = lba_endx;
    sm->cluster = lba_cluster;
}
uint32_t sm_sdio_erase_tick(struct sm_sdio_erase *sm) {
    SM_RESUME(sm);
    if (!sm->cluster) goto halt; // FIXME: this is to boot in halt
    infof("sm_sdio_erase: %d blocks at LBA %d\n", sm->endx - sm->cur, sm->cur);

    /* Erase card before writing. */
    for(; sm->cur < sm->endx; sm->cur += sm->cluster) {
        // infof("sm_sdio_erase: %x/%x\n", (int)sm->cur, (int)sm->endx);
        SM_CMD(sm, &sm->ctx, SDIO_CMD_ERASE_WR_BLK_START_ADDR, hw_sdio_card_lba_to_addr(sm->cur));
        SM_CMD(sm, &sm->ctx, SDIO_CMD_ERASE_WR_BLK_END_ADDR,   hw_sdio_card_lba_to_addr(sm->cur + sm->cluster - 1));
        SM_CMD(sm, &sm->ctx, SDIO_CMD_ERASE, 0);
        SM_WAIT(sm, hw_sdio_ready());
    }
    infof("sm_sdio_erase: done\n");
  halt:
    CB_CALL_IF(sm->done);
    SM_HALT(sm);
}





/* Chunker from BLOCK_SIZE -> SD_UNIT_SIZE (512 bytes) */
static void *sdio_block_stream_cur(void *cb) {
    struct sdio_block_stream *x = CB_OBJ(cb, struct sdio_block_stream, out_stream.cur);
    if (x->out_count < SD_UNITS_PER_BLOCK) {
        return x->buf + x->out_count * SD_UNIT_SIZE;
    }
    uint8_t *data = obj_stream_cur(x->in_stream);
    if (!data) return NULL;
    memcpy(x->buf, data, BLOCK_SIZE);
    x->out_count = 0;
    obj_stream_ack(x->in_stream);
    return x->buf;
}
static void sdio_block_stream_ack(void *cb) {
    struct sdio_block_stream *x = CB_OBJ(cb, struct sdio_block_stream, out_stream.ack);
    //LOG_EMU("count %d\n",x->out_count);
    x->out_count++;
}
void sdio_block_stream_init(struct sdio_block_stream *x,
                            struct obj_stream *in) {
    memset(x,0,sizeof(*x));
    x->out_count = SD_UNITS_PER_BLOCK; // needs refill
    x->in_stream = in;
    x->out_stream.cur.fn = sdio_block_stream_cur;
    x->out_stream.ack.fn = sdio_block_stream_ack;
}


