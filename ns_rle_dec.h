/* Format is:
   - data word
   - repeat count - 1 in LEB128 unsigned
*/

#include "assert_write.h"
#include "setjmp.h"
#include <stdint.h>

struct NS(_state) {
    uint8_t in[64 * 1024];
    NS(_t) out[1024 * 1024];
    uintptr_t in_size;
    uintptr_t in_i;
    uintptr_t out_i;
    /* Simpler to abort the loop on input EOF */
    jmp_buf done;
};
static inline void NS(_loop_init)(struct NS(_state) *s) {
    s->in_size = sizeof(s->in);
    s->in_i = sizeof(s->in);
    s->out_i = 0;
}

#define NS_RLE_DEC_FULL_STATE
#include "ns_rle_dec_exti.h"
#undef  NS_RLE_DEC_FULL_STATE

void NS(_write)(struct NS(_state) *s);

static inline void NS(_write_u8)(struct NS(_state) *s, uint8_t bus) {
    if (unlikely(s->out_i == sizeof(s->out))) {
        NS(_write)(s);
        s->out_i = 0;
    }
    s->out[s->out_i++] = bus;
}



static inline void NS(_loop)(struct NS(_state) *s) {
    if (!setjmp(s->done)) {
        NS(_loop_init)(s);
        for(;;) { /* Exits via longjmp at input EOF */
            uint8_t bus = NS(_read_u8)(s);
            uintptr_t len = NS(_read_leb128)(s);
            for (uintptr_t i=0; i<len+1; i++) {
                // LOG("write %02x\n", bus);
                NS(_write_u8)(s, bus);
            }
        }
    }
    else {
        NS(_write)(s);
    }
}
