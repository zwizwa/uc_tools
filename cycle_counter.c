#include "cycle_counter.h"
#include "macros.h"

/* These are stubs.  On bare metal targets, these are not defined.  On
   emulation targets, you probably want to provide your own
   version. */

#ifdef CYCLE_COUNTER_NO_INLINE

uint32_t cycle_counter_register = 0;

uint32_t cycle_counter(void) {
    return cycle_counter_register;
}
void enable_cycle_counter(void) {
    LOG("WARNING: cycle_counter is a stub\n");
}

#endif
