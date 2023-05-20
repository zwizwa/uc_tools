#ifndef SLICE_H
#define SLICE_H

// FIXME: This should probably be split off in an ns_slice.h instead of a macro.

#include <stdint.h>

/* This is such a common pattern.
   FIXME: start using it everywhere. */
#define DEF_SLICE_ALL(_type) \
    typedef struct { _type *buf; uintptr_t len; } slice_##_type; \
    void put_slice_##_type(slice_##_type *s, _type d) { s->buf[s->len++] = d; } \
    void write_slice_##_type(slice_##_type *s, _type *d, uintptr_t n) { for(uintptr_t i=0; i<n; i++) { put_slice_##_type(s, d[i]); } } \

#define DEF_CONST_SLICE_ALL(_type) \
    typedef struct { const _type *buf; uintptr_t len; } const_slice_##_type; \
    /* The reader consumes the slice. */ \
    void skip_const_slice_##_type(const_slice_##_type *s, uintptr_t n) { s->buf+=n; s->len-=n; } \
    _type get_const_slice_##_type(const_slice_##_type *s) { _type rv = s->buf[0]; skip_const_slice_##_type(s, 1); return rv; }


DEF_SLICE_ALL(uint8_t)
DEF_SLICE_ALL(uint16_t)
DEF_SLICE_ALL(uint32_t)
DEF_SLICE_ALL(uint64_t)
DEF_SLICE_ALL(uintptr_t)

DEF_CONST_SLICE_ALL(uint8_t)
DEF_CONST_SLICE_ALL(uint16_t)
DEF_CONST_SLICE_ALL(uint32_t)
DEF_CONST_SLICE_ALL(uint64_t)
DEF_CONST_SLICE_ALL(uintptr_t)



#endif
