#ifndef SLICE_H
#define SLICE_H

#include <stdint.h>

/* This is such a common pattern.
   FIXME: start using it everywhere. */
#define DEF_SLICE_STRUCT(_type) \
    typedef struct { _type *buf; uintptr_t len; } slice_##_type

#define DEF_CONST_SLICE_STRUCT(_type) \
    typedef struct { const _type *buf; uintptr_t len; } const_slice_##_type

DEF_SLICE_STRUCT(uint8_t);
DEF_SLICE_STRUCT(uint16_t);
DEF_SLICE_STRUCT(uint32_t);
DEF_SLICE_STRUCT(uint64_t);
DEF_SLICE_STRUCT(uintptr_t);

DEF_CONST_SLICE_STRUCT(uint8_t);
DEF_CONST_SLICE_STRUCT(uint16_t);
DEF_CONST_SLICE_STRUCT(uint32_t);
DEF_CONST_SLICE_STRUCT(uint64_t);
DEF_CONST_SLICE_STRUCT(uintptr_t);

#endif
