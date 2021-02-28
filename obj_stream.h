#ifndef OBJ_STREAM_H
#define OBJ_STREAM_H

#include "callback.h"

// Stream of generic objects, implemented as two callbacks.
struct obj_stream {
    struct cb_voidp cur;  // get current object
    struct cb_void  ack;  // free current, move pointer to next
};
INLINE void* obj_stream_cur(struct obj_stream *s) { return CB_CALL(&s->cur); }
INLINE void  obj_stream_ack(struct obj_stream *s) { return CB_CALL(&s->ack); }


#endif
