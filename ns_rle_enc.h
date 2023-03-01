
/* Format is:
   - data word
   - repeat count - 1 in LEB128 unsigned
*/

#include "macros.h"
#include <stdint.h>

struct NS(_state) {
    uintptr_t count;
    NS(_t) last;
    NS(_t) in[1024*1024];
    uint8_t out[64*1024];
    uintptr_t out_i;
};

/* User defined: input and output */
void NS(_write)(struct NS(_state) *);
ssize_t NS(_read)(struct NS(_state) *s);

static inline uintptr_t NS(_in)(struct NS(_state) *s) {
    ssize_t rv = NS(_read)(s);
    ASSERT(rv >= 0);
    return rv / sizeof(NS(_t));
}

static inline void NS(_out)(struct NS(_state) *s, uint8_t byte) {
    if (s->out_i == sizeof(s->out)) {
        NS(_write)(s);
        s->out_i = 0;
    }
    s->out[s->out_i++] = byte;
}
static inline void rle_enc_count(struct NS(_state) *s, uintptr_t sr) {
    for(;;) {
        if (sr < 128) {
            // fits -> last byte, no cont bit
            NS(_out)(s, sr);
            return;
        }
        else {
            // doesn't fit, write 7 LSB + cont bit
            NS(_out)(s,  0x80 | (sr & 0x7F));
            sr >>= 7;
        }
    }
}
static inline void NS(_data)(struct NS(_state) *s, NS(_t) bus) {
    for(uintptr_t i=0; i<sizeof(NS(_t)); i++) {
        NS(_out)(s, bus);
        bus >>= 8;
    }
}
static inline void NS(_bus)(struct NS(_state) *s, NS(_t) bus) {
    if(s->last != bus) {
        /* Flush count-1 */
        rle_enc_count(s, s->count);
        NS(_data)(s, bus);
        s->count = 0;
        s->last = bus;
    }
    else {
        s->count++;
    }
}
static inline int NS(_loop)(struct NS(_state) *s) {
    uintptr_t n = NS(_in)(s);
    ASSERT(n >= 1);
    /* First bus value */
    s->last = s->in[0];
    NS(_data)(s, s->last);
    uintptr_t i = 1;
    for(;;) {
        while(i < n) {
            NS(_bus)(s, s->in[i]);
            i++;
        }
        n = NS(_in)(s);
        /* Loop until in EOF */
        if (n == 0) {
            /* Write last count */
            rle_enc_count(s, s->count);
            NS(_write)(s);
            return 0;
        }
        i = 0;
    }
}

