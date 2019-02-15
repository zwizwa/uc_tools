#include "sm_etf.h"
#include "base.h"
#include <string.h>


//#define NEXT() SM_WAIT_BUF_READ(sm, &sm->input, u8)
#define NEXT() ({ uint8_t b = SM_WAIT_BUF_READ(sm, &sm->input, u8); infof("next: %d\n", b); b; })

// Core copy routine.
static int copy_done(struct sm_etf *sm) {
    for(;;) {
        if (sm->data_next     >= sm->data_size)     return 1;
        if (sm->input.next.u8 >= sm->input.endx.u8) return 0;
        uint8_t b = *(sm->input.next.u8)++;
        infof("copy: %d\n", b);
        sm->buf[sm->data_next++] = b;
    }
}
// Init + blocking copy
#define NEXT_CHUNK(nb) do {                     \
        sm->data_size = nb;                     \
        sm->data_next = 0;                      \
        SM_WAIT(sm, copy_done(sm));             \
    } while(0)

// All integers are big endian
static int32_t i32(struct sm_etf *sm) {
    uint8_t *b = sm->buf;
    uint32_t x = 0x100;
    uint32_t ui = b[3] + x * (b[2] + x * (b[1] + x * b[0]));
    return (int32_t)ui;
}
#define NEXT_I32() ({NEXT_CHUNK(4); i32(sm);})



// FIXME: generic buffer copy
// It's going to be simpler to use a sub-machine.

// Every read will need a buffer, and the final "fold" call will also
// need a buffer, so just have a buffer.
uint32_t sm_etf_write(struct sm_etf *sm, const uint8_t *buf, uint32_t len) {
    /* Connect buffer. */
    sm->input.next.u8 = buf;
    sm->input.endx.u8 = buf + len;
    /* Machine will run until the buffer is consumed, or an error is
       encountered. */
    return sm_etf_tick(sm);
}


uint32_t sm_etf_tick(struct sm_etf *sm) {
    SM_RESUME(sm);
    if (131 != NEXT()) return SM_ETF_ERR_PROTO;

    /* value = leaf | assoc(value)

       where assoc(value) is [{name,value]}
       where name = number

       This is enough to represent arbitrary tree data structures and
       keeps the handling simple. */


    /* The top level value is always an assoc list. */
    if (LIST_EXT != NEXT()) return SM_ETF_ERR_BAD_ENV;
    /* Ignore the count.  We assume it is nil-terminated */
    NEXT_I32();

    /* Expecting a binding.  Each binding is a pair of a number tag
     * and a value.  Number tags will be pushed onto the stack to keep
     * track of the current context. */
  next_binding: {
        uint8_t type = NEXT();
        switch(type) {
        case NIL_EXT:
            if (!sm->depth) goto done;
            sm->depth--;
            goto next_binding;
        case SMALL_TUPLE_EXT: {
            /* Binding tuples are pairs. */
            if (2 != NEXT()) return SM_ETF_ERR_BAD_PAIR;
            /* Binding tags are numbers */
            uint8_t tag = NEXT();
            switch(tag) {
            case SMALL_INTEGER_EXT: {
                uint8_t name = NEXT();
                sm->stack[sm->depth] = name;
                break;
            }
            case INTEGER_EXT: {
                int32_t name = NEXT_I32();
                sm->stack[sm->depth] = name;
                break;
            }
            default:
                return SM_ETF_ERR_BAD_NAME;
            }
            /* Resume value parsing. */
            goto next_value;
        }
        default:
            return SM_ETF_ERR_BAD_BINDING;
        }
    }

    /* Expecting a value.  This is either a leaf node or a new assoc
       list that deepens the recursion level. */
  next_value: {
        /* We are at a single value point.
           It is either a leaf ... */
        uint8_t type = NEXT();
        if (LIST_EXT == type) {
            /* Ignore the count.  We assume it is nil-terminated */
            NEXT_I32();
            /* A new list creates a new context */
            sm->depth++;
            if (sm->depth >= SM_ETF_STACK_SIZE) return SM_ETF_ERR_STACK;
        }
        else {
            switch(type) {
            case SMALL_INTEGER_EXT: {
                uint8_t b = NEXT();
                infof("small_integer_ext: %d\n", b);
                break;
            }
            case INTEGER_EXT: {
                uint32_t w = NEXT_I32();
                infof("integer_ext: %x\n", w);
                break;
            }
            default:
                return SM_ETF_ERR_PROTO;
            }

            /* We have a value in a context.  Pass it on. */
            for(int i=0; i<=sm->depth; i++) { infof("%d ", sm->stack[i]); }
            infof("<- stack\n");
        }

        goto next_binding;
    }


  done:
    SM_HALT(sm);
}
void sm_etf_init(struct sm_etf *sm, uint8_t *buf, uint32_t len) {
    memset(sm,0,sizeof(*sm));
    sm->buf = buf;
    sm->buf_size = len;
}
