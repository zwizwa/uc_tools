/* Nested data structures based on ETF.

   This supports nested dictionaries where keys are numbers and
   payloads are either numbers or binaries.  This is enough to
   represent almost anything, while avoiding symbolic names in
   small-sized firmware.
*/


#ifndef SM_ETF_H
#define SM_ETF_H
#include "sm.h"
#define SM_ETF_STACK_SIZE 10


struct sm_etf;
typedef void (*sm_etf_cb)(struct sm_etf*);
struct sm_etf {
    void *next;
    sm_etf_cb cb;
    void *cb_ctx;
    struct sm_const_buf input;
    uint8_t *buf;
    uint32_t buf_size;
    int32_t stack[SM_ETF_STACK_SIZE];
    uint32_t depth;
    uint32_t data_size;
    uint32_t data_next;
    uint8_t data_type;
};

uint32_t sm_etf_tick(struct sm_etf *sm);
uint32_t sm_etf_write(struct sm_etf *sm, const uint8_t *buf, uint32_t len);
void sm_etf_init(struct sm_etf *sm, uint8_t *buf, uint32_t len, sm_etf_cb);



#define SM_ETF_ERR_PROTO   1
#define SM_ETF_ERR_STACK   2
#define SM_ETF_ERR_BUF     3
#define SM_ETF_ERR_ENV     4
#define SM_ETF_ERR_BINDING 5
#define SM_ETF_ERR_PAIR    6
#define SM_ETF_ERR_NAME    7


#define SMALL_INTEGER_EXT  97
#define INTEGER_EXT        98
#define LIST_EXT          108
#define NIL_EXT           106
#define SMALL_TUPLE_EXT   104
#define BINARY_EXT        109

uint32_t etf_tagged_read(
    uint8_t tag,
    uint32_t (*read)(uint8_t *buf, uint32_t len),
    uint8_t *buf,
    uint32_t len);

#endif //SM_ETF_H



