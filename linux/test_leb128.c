#include "leb128.h"
#include "macros.h"

// TODO:
// - create a message sender leb128 tag format

int main(void) {
    uint8_t buf[1024];
    int32_t w0 = 1;
    for(int i=0; i<15; i++) {
        memset(buf,0,sizeof(buf));
        uintptr_t n0 = leb128_write_i32(buf, w0);
        LOG("w0=%d n0=%d\n", w0, n0);
        int32_t w1 = 0;
        uintptr_t n1 = leb128_read_i32(buf, n0, &w1);
        LOG("w1=%d n1=%d\n", w1, n1);
        ASSERT(w1 == w0);
        ASSERT(n1 == n0);
        w0 *= -3;
    }
    return 0;
}
