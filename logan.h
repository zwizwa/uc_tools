/* Logic analyzer routines. */
#ifndef LOGAN_H
#define LOGAN_H

#include "cycle_counter.h"

/* Use EXTI interrupt to record time-stamped events.  There are
   essentially two configuration options:
   - timer subdivision
   - time stamp resolution (e.g. 32 bit)

   The driver application for is a decoder for a bit rate of 250kHz,
   so the parameters will be hardcoded for that atm.

*/

// DECODE: check the number of transitions in a certain time span

#endif
