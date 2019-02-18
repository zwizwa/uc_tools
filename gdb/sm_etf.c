#include "sm_etf.h"
#include "base.h"
#include <string.h>

// These macros are private to this module, so it is OK to assume the
// 'sm' variable is defined.

#define NEXT() SM_WAIT_BUF_READ(sm, &sm->input, u8)
//#define NEXT() ({ uint8_t b = SM_WAIT_BUF_READ(sm, &sm->input, u8); infof("next: %d\n", b); b; })

// Init + blocking copy
#define NEXT_CHUNK(nb) do {                     \
        sm->data_size = nb;                     \
        sm->data_next = 0;                      \
        SM_WAIT(sm, copy_done(sm));             \
    } while(0)

#define NEXT_I32() ({NEXT_CHUNK(4); i32(sm->buf);})
#define NEXT_U32() ({NEXT_CHUNK(4); u32(sm->buf);})






// All integers are big endian
static uint32_t u32(uint8_t *b) {
    uint32_t x = 0x100;
    uint32_t ui = b[3] + x * (b[2] + x * (b[1] + x * b[0]));
    return ui;
}
static int32_t i32(uint8_t *b) {
    return (int32_t)u32(b);
}


// Core copy routine.
// FIXME: raise error when it doesn't fit.
static int copy_done(struct sm_etf *sm) {
    for(;;) {
        if (sm->data_next     >= sm->data_size)     return 1;
        if (sm->input.next.u8 >= sm->input.endx.u8) return 0;
        uint8_t b = *(sm->input.next.u8)++;
        //infof("copy: %d\n", b);
        sm->buf[sm->data_next++] = b;
    }
}


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

  next_term:
    if (131 != NEXT()) return SM_ETF_ERR_PROTO;

    /* value = leaf | assoc(value)

       where assoc(value) is [{name,value]}
       where name = number

       This is enough to represent arbitrary tree data structures and
       keeps the handling simple. */


    /* Expecting a value.  This is either a leaf node or a new assoc
       list that deepens the recursion level. */
  next_value: {
        sm->data_type = NEXT();
        if (LIST_EXT == sm->data_type) {
            /* Ignore the count.  We assume it is nil-terminated */
            NEXT_I32();
            /* A new list creates a new context */
            sm->depth++;
            if (sm->depth >= SM_ETF_STACK_SIZE) return SM_ETF_ERR_STACK;
        }
        else {
            switch(sm->data_type) {
            case SMALL_INTEGER_EXT: {
                uint8_t byte = NEXT();
                //infof("si:%d\n", byte);
                sm->data_size = 1;
                sm->buf[0] = byte;
                break;
            }
            // SMALL_BIG_EXT, e.g. for u64
            case INTEGER_EXT: {
                int32_t word = NEXT_I32();
                //infof("i:%d\n", word);
                sm->data_size = 4;
                memcpy(sm->buf, &word, sm->data_size);
                break;
            }
            case BINARY_EXT: {
                uint32_t len = NEXT_U32();
                //infof("b:%d\n", len);
                if (len > sm->buf_size) return SM_ETF_ERR_BUF;
                NEXT_CHUNK(len);
                break;
            }
            default:
                return SM_ETF_ERR_PROTO;
            }

            /* We have a value in a context.  FIXME: Put the callback here. */
            if(sm->cb) {
                uint32_t rv = sm->cb(sm);
                if (rv) return rv;
            }
        }

        goto next_binding;
    }


    /* Expecting a binding.  Each binding is a pair of a number tag
     * and a value.  Number tags will be pushed onto the stack to keep
     * track of the current context. */
  next_binding: {
        if (!sm->depth) goto done;

        sm->data_type = NEXT();
        switch(sm->data_type) {
        case NIL_EXT:
            /* Acts as "commit" command for a dictionary. */
            sm->data_size = 0;
            if(sm->cb) {
                uint32_t rv = sm->cb(sm);
                if (rv) return rv;
            }
            sm->depth--;
            goto next_binding;
        case SMALL_TUPLE_EXT: {
            /* Binding tuples are pairs. */
            if (2 != NEXT()) return SM_ETF_ERR_PAIR;
            /* Binding tags are numbers */
            uint8_t tag = NEXT();
            switch(tag) {
            case SMALL_INTEGER_EXT: {
                uint8_t name = NEXT();
                sm->stack[sm->depth-1] = name;
                break;
            }
            case INTEGER_EXT: {
                int32_t name = NEXT_I32();
                sm->stack[sm->depth-1] = name;
                break;
            }
            default:
                return SM_ETF_ERR_NAME;
            }
            /* Resume value parsing. */
            goto next_value;
        }
        default:
            return SM_ETF_ERR_BINDING;
        }
    }


  done:
    /* Don't halt.  Just keep going. */
    goto next_term;


    SM_HALT(sm);
}
void sm_etf_init(struct sm_etf *sm, uint8_t *buf, uint32_t len, sm_etf_cb cb) {
    memset(sm,0,sizeof(*sm));
    sm->buf = buf;
    sm->buf_size = len;
    sm->cb = cb;
}



/* Not part of read SM, but convenient. */
uint32_t etf_tagged_read(uint8_t tag, uint32_t (*read)(uint8_t *buf, uint32_t len),
                         uint8_t *buf, uint32_t len) {
    uint32_t data_size = read(buf+15, len-15-1);
    if (!data_size) return 0;
    buf[0] = 131;
    buf[1] = LIST_EXT;
    buf[2] = 0;
    buf[3] = 0;
    buf[4] = 0;
    buf[5] = 1;
    buf[6] = SMALL_TUPLE_EXT;
    buf[7] = 2;
    buf[8] = SMALL_INTEGER_EXT;
    buf[9] = tag;
    buf[10] = BINARY_EXT;
    buf[11] = data_size >> 24;
    buf[12] = data_size >> 16;
    buf[13] = data_size >> 8;
    buf[14] = data_size >> 0;
    buf[15 + data_size] = NIL_EXT;
    return 15 + data_size + 1;
}
