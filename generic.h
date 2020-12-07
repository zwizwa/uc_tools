#ifndef GENERIC_H
#define GENERIC_H

// Misc tools shared between STM32F103 and Linux C.
// FIXME: Phase this out.  Use only "macros.h", and put the rest into specific header files.

#include <stdint.h>
#include "macros.h"


#define TRY(expr) { int err = expr; if(err) return err; }

/* Treat 0xFFFFFFFF as NULL to work with uninitialized Flash */
#define NULL_00 ((void *)0x00000000UL)
#define NULL_FF ((void *)0xFFFFFFFFUL)
INLINE int flash_null(const void *ptr) {
    return ((ptr == NULL_00) || ptr == NULL_FF);
}
INLINE const char* flash_string(const char *str, const char *defstr) {
    return flash_null(str) ? defstr : str;
}

#ifndef offsetof
#define offsetof(st,m) __builtin_offsetof(st,m)
#endif

#ifndef typeof
#define typeof(x)      __typeof__(x)
#endif


// Useful for fallthrough routing.
#define RETURN_NZ(expr) \
    do { int _rsize = expr; if (_rsize) return _rsize; } while(0)


#include "callback.h"

// Stream of generic objects, implemented as two callbacks.
struct obj_stream {
    struct cb_voidp cur;  // get current object
    struct cb_void  ack;  // free current, move pointer to next
};
INLINE void* obj_stream_cur(struct obj_stream *s) { return CB_CALL(&s->cur); }
INLINE void  obj_stream_ack(struct obj_stream *s) { return CB_CALL(&s->ack); }

static inline uint32_t min_u32(uint32_t a, uint32_t b) { return a < b ? a : b;; }
static inline uint32_t max_u32(uint32_t a, uint32_t b) { return a > b ? a : b;; }

INLINE int64_t sq64(int64_t a) { return a*a; }

// How many size den packets do you need to fit num bytes?
INLINE int32_t div_ceil(uint32_t num, uint32_t den) {
    if (num == 0) { return 0; }
    else { return 1 + (num-1)/den; }
}
// How many pad bytes are left over?
INLINE int32_t mod_ceil(uint32_t num, uint32_t den) {
    return div_ceil(num, den) * den - num;
}

INLINE uint32_t nb_bits(uint32_t word) {
    uint32_t count = 0;
    while(word) {
        count += (word & 1);
        word >>= 1;
    }
    return count;
}

INLINE int islower(int i) {
    return (i >= 'a') && (i <= 'z');
}


#endif

