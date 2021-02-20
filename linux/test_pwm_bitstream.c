#include "gdb/pwm_bitstream.h"
#include "macros.h"
#include <stdint.h>

void log_bits(uint8_t *buf, uint32_t nb_bits) {
    for (int i=0; i<nb_bits; i++) {
        int byte = i / 8;
        int bit  = i % 8;
        int bit_val = (buf[byte] >> (7 - bit)) & 1;
        //LOG("%s", bit_val ? "'" : ".");
        LOG("%d", bit_val);
    }
    LOG("\n");
}

int main(void) {

    uint8_t buf[1024];

    /* Generic bitbuf test.  */
    for (int n=1; n<32; n++) {
        struct bitbuf dst;
        bitbuf_init(&dst, buf);
        for (int i=0; i<n; i++) {
            int val = i > n-4;
            bitbuf_write(&dst, val);
        }
        uint32_t nb_bytes = bitbuf_close(&dst);
        ASSERT((((n-1)/8)+1) == nb_bytes);
        uint32_t n_round = nb_bytes * 8;
        log_bits(buf, n_round);
    }

    /* PWM bitstream (WS2812) */
    uint8_t grb[3] = {0,0,4};
    uint32_t nb_bytes =
        pwm_bitstream_write_pad(
            buf, grb,
            24 /* nb input bits, resulting in 72 output bits, or 9 bytes. */,
            0 /* won't be used as we are already aligned with byte grid. */
            );
    ASSERT(9 == nb_bytes);
    LOG("%d:", nb_bytes);
    for(int i=0; i<nb_bytes; i++) {
        LOG(" %02x", buf[i]);
    }
    LOG("\n");
    log_bits(buf, 24*3);


    return 0;
}
