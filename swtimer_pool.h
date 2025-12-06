#ifndef SWTIMER_POOL_H
#define SWTIMER_POOL_H

/* Software timer + event object pool.

   The swtimer.h implementation associates 16-bit timestamps to 16-bit
   user tags.  Tags are kept small because they are moved around
   during tree rebalancing. The idea is to store data in the tag
   directly if there are enough bits, or otherwise use the tags as
   references to event payload stored elswehere.

   Here we implement an object pool for the latter case.

   FIXME: Currently some things are hardcoded.  Change it to a proper
   NS implementation later with this file as a special case.

*/


#ifndef SWTIMER_EVENT_TYPE
#error Need SWTIMER_EVENT_TYPE
#endif

#ifndef SWTIMER_POOL_SIZE
#error Need SWTIMER_POOL_SIZE
#endif

#if SWTIMER_POOL_SIZE <= 256
typedef uint8_t swtimer_event_index_t;
#else
typedef uint16_t swtimer_event_index_t;
#endif

CT_ASSERT(swtimer_event_index_size,
          (1<<(sizeof(swtimer_event_index_t)*8)) >= SWTIMER_POOL_SIZE);

#include "swtimer.h"

typedef struct {
    swtimer_event_index_t top;
    swtimer_event_index_t stack[SWTIMER_POOL_SIZE];
} swtimer_event_free_stack_t;
typedef swtimer_event_index_t swtimer_event_free_element_t;
#define NS(s) swtimer_event_free##s
#include "ns_stack.h"
#undef NS


struct swtimer_pool {
    struct swtimer swtimer;
    swtimer_element_t arr[SWTIMER_POOL_SIZE];
    swtimer_event_free_stack_t free;
    SWTIMER_EVENT_TYPE pool[SWTIMER_POOL_SIZE];
};

static inline void swtimer_pool_reset(struct swtimer_pool *swt) {
    /* See test_heap.c */
    swt->swtimer.nb = 0;
    swt->swtimer.arr = swt->arr;
    swtimer_event_free_pool_init(&swt->free, SWTIMER_POOL_SIZE);

}

static uint16_t swtimer_pool_alloc_event_tag(struct swtimer_pool *swtimer_pool) {
    return swtimer_event_free_pop(&swtimer_pool->free);
}
static void swtimer_pool_free_event_tag(struct swtimer_pool *swtimer_pool, uint16_t event_tag) {
    swtimer_event_free_push(&swtimer_pool->free, event_tag);
}
static SWTIMER_EVENT_TYPE* swtimer_pool_event(struct swtimer_pool *swtimer_pool, uint16_t event_tag) {
    SWTIMER_EVENT_TYPE *e = &swtimer_pool->pool[event_tag];
    return e;
}


#endif
