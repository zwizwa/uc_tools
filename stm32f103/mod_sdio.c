#ifndef MOD_SDIO
#define MOD_SDIO

/* Towards an SD card interace.

   Goal here is twofold: basic SD card access, and exploration of FPGA
   "fast path" setup, where a single FPGA manages a number of SD cards
   and can switch the "view" for the microcontroller to expose one at
   a time.

*/

#include "hw_sdio.h"
#include "sdio_cmd.h"
#include "sm_sdio.h"


#include "instance.h"

#include "cycle_counter.h"

#define LOG_EXPR(fmt, expr) LOG(fmt, #expr, expr)
#define LOG_EXPR_D(expr) LOG_EXPR("%s = %d\n", expr)
#define LOG_EXPR_X(expr) LOG_EXPR("%s = 0x%08x\n", expr)

/* Access to sdio port is provided by a "monitor", which handles
   mutual exclusion by handling one request at a time. */
struct sdio_monitor {
    void *next;
    union {
        struct sm_sdio_write_cluster sm_sdio_write_cluster;
    } sub;
    struct transfer_context transfer_context;
    struct device device;
    struct hw_sdio_card hw_sdio_card;
    uint32_t request;
};
void sdio_monitor_init(struct sdio_monitor *s) {
    ZERO(s);
    /* Init the underlying hardware. */
    uint32_t timeout = cycle_counter_future_time(72*1000*500);
    uint32_t error;

    for(;;) {
        if (!(error = hw_sdio_open(&s->hw_sdio_card))) break;
        if (cycle_counter_expired(timeout)) break;
    }
    if (error) {
        infof("sd: card timeout\n");
    }
    else {
        s->device.impl.sd = &s->hw_sdio_card;
        s->transfer_context.device = &s->device;
    }

}

static inline int request(uint32_t *vec, uint32_t req) {
    uint32_t mask = (1 << req);
    if ((*vec) & mask) {
        *vec &= ~mask;
        return 1;
    }
    else {
        return 0;
    }
    return 1;
}
#define SDIO_MONITOR_REQUEST_READ 0

#define SDIO_LOGSIZE 9  // 512
uint8_t tmp_buf[1 << SDIO_LOGSIZE];
void tmp_stream_ack(void *ctx) { }
void *tmp_stream_cur(void *ctx) { return tmp_buf; }
struct obj_stream tmp_stream = {
    .cur = {.fn = tmp_stream_cur}, .ack = {.fn = tmp_stream_ack}
};

sm_status_t sdio_monitor_tick(struct sdio_monitor *s) {
    SM_RESUME(s);
    /* Once init is complete, the device pointer is set.  If init
     * fails, we just block here in case we are polled. */
    SM_WAIT(s, s->transfer_context.device);
    infof("device ok\n");

  loop:
    if (request(&s->request, SDIO_MONITOR_REQUEST_READ)) {
        infof("write cluster: start\n");
        uint32_t nb_blocks = 10000;
        uint32_t lba = 8;
        s->transfer_context.block_stream = &tmp_stream;
        sm_status_t status = SM_SUB_CATCH(
            s, sm_sdio_write_cluster,
            &s->transfer_context, nb_blocks, SDIO_LOGSIZE, lba);
        LOG("write cluster: status = %x\n", status);
    }
    SM_SUSPEND(s);
    goto loop;
}


/* Instance */

struct sdio_monitor sdio_monitor;

DEF_COMMAND(sdio_req) {
    sdio_monitor.request |= 1;
}
void sdio_monitor_poll(void) {
    sdio_monitor_tick(&sdio_monitor);
}


instance_status_t sdio_init(instance_init_t *ctx) {
    hw_sdio_init();

    sdio_monitor_init(&sdio_monitor);

    //LOG_EXPR_X(sd.nb_units);
    _service.add(sdio_monitor_poll);


    return 0;
}

DEF_INSTANCE(sdio);

#endif
