/* There are Rust and C++ counterparts to this.  However, currently I
   am stuck in low-resource C land, so I am revisiting
   requirements.

   - A small DAQ head that can run in EXTI ISR on STM32F103,
     differential logging, spill to USB

   - Parsers that can operate on differential data.  This seems to
     require two operations:

     - Wind forward in time and get value

     - Wind to next event and measure time diff

   - Parsers are written in "push style", presented with one event at
     a time, exposed as a static inline function to allow inlining
     when composing machines.

   Looking at in-place processing to simplify memory management and
   increase data locality for speed.  This probably means that
   analyzers have to be generated and specialized to task.

   So we are looking at DSL + primitives.

*/

#include "macros.h"
#include <stdint.h>

/* All protocol analysis will probably happen on 64 bit host, so data
   structures should be designed to be native.  Let's stick to time as
   uintptr_t, which is 71 minutes @ 1MHz on 32-bit, still plenty to
   run some protocol analysis on a 32-bit microcontroller. */
typedef uint32_t la_time_t;

/* An event is a data sample in time.  Processors should not assume
   that a value changes between events. */
struct la_event {
    la_time_t time;
    union {
        uintptr_t uptr;
    } value;
};

/* For representation on uC, a bit-packet representation is probably
   more appropriate, e.g. 1 bit value + 15 bit relative time, or 2 bit
   value + 14. bit relative time. */

/* A UART parser */
struct la_uart {
    /* Not clear what is best.  To use 8-bit or machine words?  This
       is going to end up as part of inner loop state, so likely will
       be implmented as registers.  For now, let's stick to 8-bit,
       then later change when performance metrics are available. */
    uint16_t shiftreg;
    uint8_t state;
    uint8_t nb_start;
    uint8_t nb_stop;
    uint8_t nb_data;
};

int main(int argc, char **argv) {
    LOG("%s\n", argv[0]);
    return 0;
}
