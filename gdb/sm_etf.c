#include "sm_etf.h"
#include "base.h"
#include <string.h>

#ifdef ASSERT
#undef ASSERT
#endif

#define ASSERT(cond) do {if (!(cond)) return SM_ETF_ERR_PROTO; } while(0)
#define NEXT() SM_WAIT_BUF_READ(sm, &sm->input, u8)
//#define NEXT() ({ uint8_t b = SM_WAIT_BUF_READ(sm, &sm->input, u8); infof("next: %d\n", b); b; })

uint32_t sm_etf_write(struct sm_etf *sm, const uint8_t *buf, uint32_t len) {
    sm->input.next.u8 = buf;
    sm->input.endx.u8 = buf + len;

    /* Machine will run until the buffer is consumed, or an error is
       encountered. */
    return sm_etf_tick(sm);
}

uint32_t sm_etf_tick(struct sm_etf *sm) {
    SM_RESUME(sm);
    ASSERT(131 == NEXT());
    SM_HALT(sm);
}
void sm_etf_init(struct sm_etf *sm) {
    memset(sm,0,sizeof(*sm));
}
