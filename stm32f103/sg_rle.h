#ifndef SG_RLE_H
#define SG_RLE_H

/* STM32F103 signal generator using 8-bit RLE encoding

   This code needs cycle_counter() and cycle_counter_expired() from
   STM lib and references iwdg_reset() to reset the watchdog timer
   inside the bitbang loop.

*/

#include <stdint.h>

struct sg_rle_ctx {
    // Bit-band address for TX pin: low, hih
    volatile uint32_t *tx[2];
};


void sg_rle_8(struct sg_rle_ctx *c,
              const uint8_t *rle_ops,
              uint32_t nb) {
    /* Tested on STM32F103 @72MHz, the 2MHz sampling seems to be too
       tight.  Maybe optimize in assembly to make it work.  For now
       1MBit is probably ok. */
    // uint32_t dt = HW_CPU_MHZ / 2;
    uint32_t dt = HW_CPU_MHZ;
    uint32_t t = cycle_counter() + dt;

    /* First flip sets 0.  FIXME: This should probably perform a GPIO
       read to reflect actual pin state. */

    uint32_t state = 1;
    for (int j = 0; j<nb; j++) {

        iwdg_reset();

        uint8_t rle_op = rle_ops[j];

        /* There are two rle opcodes: perform bitflip with delay, and
           pure delay without any other operations. */

        if (rle_op & 0x80) {
            /* Do flip */

            /* Note that we always wait one bit length _before_ making
               the line change, while the semantics of the 7bit delay
               is to wait _after_ the change.  This works because we
               chain things together.  We need to wait before the
               change to accurately align timing to the time grid. */

            state ^= 1;
            volatile uint32_t *p = c->tx[state];
            while (!cycle_counter_expired(t));
            *p = 1;
            t += dt;
            // Pad it with extra wait bits.
            uint8_t extra_wait = rle_op & 0x7f;
            while(extra_wait--) {
                while (!cycle_counter_expired(t));
                t += dt;
            }
        }
        else {
            /* Do pure delay */

            /* This can use a different time scale.
               See lua compiler code. */
            uint32_t extra_wait = (rle_op & 0x7f) + 1;
            while(extra_wait > 0) {
                for(int i=0; i<128; i++) {
                    while (!cycle_counter_expired(t));
                    t += dt;
                }
                extra_wait--;
            }
        }
    }
}



#endif
