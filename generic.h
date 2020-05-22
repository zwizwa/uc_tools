#ifndef GENERIC_H
#define GENERIC_H

// Base tools shared between STM32F103 and Linux C.

#include <stdint.h>
#include "macros.h"

/* Note that "static inline" doesn't necessarily inline.  Adding this
   attribute will force GCC to inline the function even if
   optimization is off. */
#ifndef INLINE
#define INLINE static inline __attribute__((__always_inline__))
#endif

/* The default error propagation is to exit the program.  This works
 * well for the intended use as Erlang port.  Note that for
 * microcontroller use this should probably be overridden. */
#ifndef ABORT
#define ABORT exit(1)
#endif

#ifndef ERROR
#define ERROR(...) do {LOG(__VA_ARGS__);ABORT;}while(0)
#endif

#define CT_ASSERT(name, pred) \
    typedef char nct_assert_##name[(pred) ? 1 : -1]
#define CT_ASSERT_STRUCT_SIZE(name, size) \
    CT_ASSERT(struct_##name, sizeof(struct name) == size)
#define CT_ASSERT_UNION_SIZE(name, size) \
    CT_ASSERT(union_##name, sizeof(union name) == size)

#ifndef ASSERT
#define ASSERT(assertion) ({ \
            if(!(assertion)) { \
                LOG("%s: %d: ASSERT FAIL: " #assertion "\n", __FILE__, __LINE__); \
                ABORT; \
            } })
#endif
#define ASSERT_EQ(a,b) ({ \
            __typeof__(a) _a = (a); \
            __typeof__(b) _b = (b); \
            if(_a != _b) { \
                LOG("ASSERT FAIL: " #a "(%d) == " #b "(%d)\n", _a, _b); \
                ABORT; \
            } })
#define ASSERT_GT(a,b) ({ \
            __typeof__(a) _a = (a); \
            __typeof__(b) _b = (b); \
            if(_a <= _b) { \
                LOG("ASSERT FAIL: " #a "(%d) <= " #b "(%d)\n", _a, _b); \
                ABORT; \
            } })

#define FOR_ARRAY(a,p) \
    for(typeof(&a[0]) p = &a[0]; p < &a[ARRAY_SIZE(a)]; p++)


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

