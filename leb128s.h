#ifndef LEB128S_H
#define LEB128S_H

/* Streaming reader/writer for leb128-based binary tree format.

   Note that the tree format uses signed encoding for all integers,
   but tag_u32's struct is unsigned.
*/

#include "leb128.h"
#include "tag_u32.h"

/* Error propagation.  See header file for comments. */
#include "or_abort.h"

#define T_INT 1
#define T_TAG 7

#define LEB128S_ERROR_ALLOC 1
#define LEB128S_ERROR_UNKNOWN_TAG 2

/* Byte stream representing LEB128-based tree encoding.  Use a short
   name for the struct and the function prefix, as it is used a
   lot. */
struct leb128s_env;
typedef intptr_t leb128s_status_t;
struct leb128s {
    uint8_t*  buf;
    uintptr_t len;
    uintptr_t offset;
    leb128s_status_t error;
    struct leb128s_env *env;
};
/* For use in the fold. */
struct leb128s_env {
    leb128s_status_t (*tag_u32)(struct leb128s_env *, struct tag_u32 *);
};

static inline int32_t leb128s_i32(struct leb128s *s) {
    int32_t val = 0;
    s->offset += leb128_read_i32(s->buf + s->offset, s->len - s->offset, &val);
    return val;
}
/* This is used a lot so wrap it into a macro. */
#define LEB128S_I32(s,v) \
    ({int32_t val = leb128s_i32(s); OR_ABORT(s, v); val; })

static inline int32_t *leb128s_i32_array(struct leb128s *s, int32_t nb, int32_t *arr) {
    for (int i=0; i<nb; i++) {
        arr[i] = LEB128S_I32(s,NULL);
    }
    return arr;
}

static inline struct tag_u32 *leb128s_read_tag_u32(
    struct leb128s *s, struct tag_u32 *msg, int32_t *tags, int32_t nb_tags) {

    msg->nb_from = LEB128S_I32(s, NULL);
    if (msg->nb_from >= nb_tags) goto alloc_error;
    msg->from = (uint32_t*)leb128s_i32_array(s, msg->nb_from, tags); OR_ABORT(s, NULL);

    tags    += msg->nb_from;
    nb_tags -= msg->nb_from;

    msg->nb_args = LEB128S_I32(s, NULL);
    if (msg->nb_args >= nb_tags) goto alloc_error;
    msg->args = (uint32_t*)leb128s_i32_array(s, msg->nb_args, tags); OR_ABORT(s, NULL);

    msg->nb_bytes = LEB128S_I32(s, NULL);
    msg->bytes = s->buf + s->offset;
    s->offset += msg->nb_bytes;

    return msg;

  alloc_error:
    s->error = LEB128S_ERROR_ALLOC;
    return NULL;
}


/* Read a single element.  Note that this can be a composite element,
   for which we need to weave in an iterator (a fold). */
static inline int32_t leb128s_element(struct leb128s *s) {
    int32_t tag = leb128s_i32(s); OR_ABORT(s,-1);
    switch(tag) {
    case T_TAG: {
        /* We don't know how many tags there are, so just guess.
           FIXME: Maximum space depends on context in microcontroller
           setting. */
        int32_t nb_tags = 16;
        int32_t tags[nb_tags];
        struct tag_u32 msg = {};
        leb128s_read_tag_u32(s, &msg, tags, nb_tags); OR_ABORT(s,-1);

        if (s->env && s->env->tag_u32) {
            s->env->tag_u32(s->env, &msg);
        }
        break;
    }
    default:
        ERROR("unknown tag %d\n", tag);
        s->error = LEB128S_ERROR_UNKNOWN_TAG;
        tag = -1;
    }
    return tag;
}

#endif
