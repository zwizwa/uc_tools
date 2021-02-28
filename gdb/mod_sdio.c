#ifndef MOD_SDIO
#define MOD_SDIO

#include "hw_sdio.h"
#include "sdio_cmd.h"
#include "sm_sdio.h"


#include "instance.h"

#include "cycle_counter.h"

#define LOG_EXPR(fmt, expr) LOG(fmt, #expr, expr)
#define LOG_EXPR_D(expr) LOG_EXPR("%s = %d\n", expr)
#define LOG_EXPR_X(expr) LOG_EXPR("%s = 0x%08x\n", expr)

struct hw_sdio_card sd;

instance_status_t sdio_init(instance_init_t *ctx) {
    hw_sdio_init();

    uint32_t timeout = cycle_counter_future_time(72*1000*500);

    uint32_t error;
    for(;;) {
        if (!(error = hw_sdio_open(&sd))) break;
        if (cycle_counter_expired(timeout)) break;
    }
    if (error) {
        infof("sd: card timeout\n");
    }
    //LOG_EXPR_X(sd.nb_units);


    return 0;
}

DEF_INSTANCE(sdio);

#endif
