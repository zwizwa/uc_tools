/* Format is:
   - data word
   - repeat count - 1 in LEB128 unsigned
*/

#include "assert_write.h"
#include <stdint.h>

typedef uint8_t data_t;

data_t in[64 * 1024];
data_t out[1024 * 1024];

uintptr_t in_size = sizeof(in);
uintptr_t in_i = sizeof(in);
uintptr_t out_i = 0;

void out_flush(void) {
    assert_write(1, (uint8_t*)out, out_i * sizeof(data_t));
    out_i = 0;
}
void in_update(void) {
    ssize_t rv = read(0, in, sizeof(in));
    if (rv == 0) {
        out_flush();
        exit(0);
    }
    ASSERT(rv > 0);
    in_i = 0;
    in_size = rv / sizeof(data_t);
}
static inline uint8_t in_read_u8(void) {
    if (unlikely(in_i == in_size)) { in_update(); }
    return in[in_i++];
}
static inline uintptr_t in_read_leb128(void) {
    uintptr_t sr = 0;
    uint8_t shift = 0;
    for (;;) {
        uintptr_t u8 = in_read_u8();
        sr |= (u8 & 0x7f) << shift;
        shift += 7;
        if (!(u8 & 0x80)) break;
    }
    // LOG("sr=%llu\n", sr);
    return sr;
}
static inline void out_write_u8(uint8_t bus) {
    if (unlikely(out_i == sizeof(out))) { out_flush(); }
    out[out_i++] = bus;
}

int main(int argc, char **argv) {
    for(;;) {
        uint8_t bus = in_read_u8();
        uintptr_t len = in_read_leb128();
        for (uintptr_t i=0; i<len+1; i++) {
            // LOG("write %02x\n", bus);
            out_write_u8(bus);
        }
    }
}
