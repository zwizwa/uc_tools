#ifndef ACTOR_H
#define ACTOR_H
#include "macros.h"
#include <stdint.h>

/* ns_actor.h instantiation for pointer-sized message type. */

typedef struct {
    void *p;
    uintptr_t up;
} actor_element_t;
typedef actor_element_t actor_oob_element_t;
static inline actor_oob_element_t actor_oob_element_none(void) {
    actor_oob_element_t none = {.up = 0};
    return none;
}

typedef struct {
    volatile uint32_t write;
    volatile uint32_t read;
    uint32_t size;
    volatile actor_element_t *buf;
#ifdef CBUF_DEBUG
    volatile uint32_t watermark;
    volatile uint32_t overflow;
#endif
} actor_queue_t;

#define NS(name) CONCAT(actor,name)
#include "ns_actor.h"
#undef NS

#define ACTOR_TASK_INIT(t,r,b) \
    NS_ACTOR_TASK_INIT(t,(actor_resume_f)r, b)

#define ACTOR_RESUME(t) \
    if ((t)->next) { goto *((t)->next); }
#define ACTOR_RECV(t)                           \
    ACTOR_RECV_LABEL(t,GENSYM(actor_recv))
#define ACTOR_RECV_LABEL(t,label)                       \
    ({                                                  \
      label:                                            \
        if(!actor_elements(&(t)->task.mbox)) {          \
            t->next = &&label;                          \
            return;                                     \
        }                                               \
        actor_get(&(t)->task.mbox); })

#endif
