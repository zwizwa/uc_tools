/* Subset of ns_rle_dec.h that is needed to implement file input to
   external iterator (i.e. do not expand to flat file) */

#include "setjmp.h"
#include <stdint.h>


#ifndef NS_RLE_DEC_FULL_STATE
/* Reduced state struct: no out members needed. */
struct NS(_state) {
    uint8_t in[64 * 1024];
    uintptr_t in_size;
    uintptr_t in_i;
    /* Simpler to abort the loop on input EOF */
    jmp_buf done;
    /* Room for a file descriptor. */
    int fd;
};
static inline void NS(_loop_init)(struct NS(_state) *s) {
    s->in_size = sizeof(s->in);
    s->in_i = sizeof(s->in);
}
#endif

ssize_t NS(_read)(struct NS(_state) *s);
static inline void NS(_read_buffer)(struct NS(_state) *s) {
    ssize_t rv = NS(_read)(s);
    if (rv == 0) { longjmp(s->done, 1); }
    ASSERT(rv > 0);
    s->in_i = 0;
    s->in_size = rv;
}
static inline uint8_t NS(_read_u8)(struct NS(_state) *s) {
    if (unlikely(s->in_i == s->in_size)) { NS(_read_buffer)(s); }
    return s->in[s->in_i++];
}
static inline uintptr_t NS(_read_leb128)(struct NS(_state) *s) {
    uintptr_t sr = 0;
    uint8_t shift = 0;
    for (;;) {
        uintptr_t u8 = NS(_read_u8)(s);
        sr |= (u8 & 0x7f) << shift;
        shift += 7;
        if (!(u8 & 0x80)) break;
    }
    // LOG("sr=%llu\n", sr);
    return sr;
}

