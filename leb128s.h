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
    leb128s_status_t (*tag_u32)(struct leb128s *, struct tag_u32 *);
    leb128s_status_t (*i32)(struct leb128s *, int32_t);
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
        if (arr) { // allow skip if NULL
            arr[i] = LEB128S_I32(s,NULL);
        }
    }
    return arr;
}

static inline struct tag_u32 *leb128s_read_tag_u32(
    struct leb128s *s, struct tag_u32 *msg, int32_t *tags, int32_t nb_tags) {

    msg->nb_from = LEB128S_I32(s, NULL);
    if (msg->nb_from >= nb_tags) goto alloc_error;
    msg->from = (uint32_t*)leb128s_i32_array(s, msg->nb_from, tags); OR_ABORT(s, NULL);

    if (tags) { // allow skip if NULL
        tags += msg->nb_from;
    }
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

/* A note on recursion.

   This needs to run on a microcontroller with static memory and
   limited stack space for temporary variables.  The intention is to
   always use a fold to process the hierarchical data structure on the
   fly, and never reify the data structure.

   I've been looking for a while on how to properly embed this idea
   (from functional programming, which usually assumes infinite memory
   models), and map it to a setting where memory allocation is an
   ordeal.

   The solution seems to be to explicitly assume the following:

   - Stick to the idea of recursion and "constructor-replacing fold".
     It is too pretty to give up.

   - Make construction explicit.  This allows e.g. to use a bump
     allocator that can manage limited lifetime of objects, or a
     global memory pool.

   The context needed to perform the allocation is then threaded
   linearly through the recursive descent.

   The only thing we need to know here is that objects that are passed
   to the recursive variants use generic ids to refer to objects
   inside that explicit (temporary) allocator.

   leb128s_id_t is that abstract reference.

*/

typedef intptr_t leb128_id_t;

static inline leb128_id_t leb128s_element(struct leb128s *s) {
    int32_t tag = leb128s_i32(s); OR_ABORT(s, 0);
    ASSERT(s->env);
    switch(tag) {
    case T_INT: {
        int32_t val = LEB128S_I32(s, 0);
        if (s->env->i32) {
            return s->env->i32(s, val);
        }
        break;
    }
    case T_TAG: {
        /* We don't know how many tags there are, so just guess.
           FIXME: Maximum space depends on context in microcontroller
           setting. */
        int32_t nb_tags = 16;
        int32_t tags[nb_tags];
        struct tag_u32 msg = {};
        leb128s_read_tag_u32(s, &msg, tags, nb_tags); OR_ABORT(s, 0);
        if (s->env->tag_u32) {
            return s->env->tag_u32(s, &msg);
        }
        break;
    }
    default:
        ERROR("unknown tag %d\n", tag);
        s->error = LEB128S_ERROR_UNKNOWN_TAG;
        break;
    }
    /* Fallthrough means that there was no handler for the tag, so we
       return the null reference, or an error condition if s->error
       was set. */
    return 0;
}

#endif
