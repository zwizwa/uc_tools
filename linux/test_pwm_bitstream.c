#include "gdb/pwm_bitstream.h"
#include "macros.h"
#include <stdint.h>

int main(void) {
    uint8_t grb[3] = {0,0,4};
    uint8_t buf[1024];
    uint32_t nb_bytes =
        pwm_bitstream_write(
            buf, grb,
            24 /* nb input bits, resulting in 72 output bits, or 9 bytes. */);
    LOG("%d:", nb_bytes);
    for(int i=0; i<nb_bytes; i++) {
        LOG(" %02x", buf[i]);
    }
    LOG("\n");
    return 0;
}
