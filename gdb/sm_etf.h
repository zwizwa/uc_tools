#ifndef SM_ETF_H
#define SM_ETF_H
#include "sm.h"
#define SM_ETF_STACKSIZE 10
struct sm_etf {
    void *next;
    struct sm_const_buf input;
    uint32_t stack[SM_ETF_STACKSIZE];
};
uint32_t sm_etf_tick(struct sm_etf *sm);
uint32_t sm_etf_write(struct sm_etf *sm, const uint8_t *buf, uint32_t len);
void sm_etf_init(struct sm_etf *sm);

#define SM_ETF_ERR_PROTO 1

#endif //SM_ETF_H
