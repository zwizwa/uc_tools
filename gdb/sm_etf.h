#ifndef SM_ETF_H
#define SM_ETF_H
#include "sm.h"
#define SM_ETF_STACK_SIZE 10


struct sm_etf {
    void *next;
    struct sm_const_buf input;
    uint8_t *buf;
    uint32_t buf_size;
    uint8_t data_size;
    uint8_t data_next;
    int32_t stack[SM_ETF_STACK_SIZE];
    uint32_t depth;
};
uint32_t sm_etf_tick(struct sm_etf *sm);
uint32_t sm_etf_write(struct sm_etf *sm, const uint8_t *buf, uint32_t len);
void sm_etf_init(struct sm_etf *sm, uint8_t *buf, uint32_t len);

#define SM_ETF_ERR_PROTO       1
#define SM_ETF_ERR_STACK       2
#define SM_ETF_ERR_BAD_ENV     3
#define SM_ETF_ERR_BAD_BINDING 4
#define SM_ETF_ERR_BAD_PAIR    5
#define SM_ETF_ERR_BAD_NAME    6


#define SMALL_INTEGER_EXT  97
#define INTEGER_EXT        98
#define LIST_EXT          108
#define NIL_EXT           106
#define SMALL_TUPLE_EXT   104

#endif //SM_ETF_H
