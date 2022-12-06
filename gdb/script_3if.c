/* The idea here is to revive the old microcontroller script idea.
   A lot of code fits the following pattern:
   - Compile a C function to ARM code
   - Load it into uc RAM
   - Execute it
   - Go back to bootloader loop

   The main advantage of that is that the code on the microcontroller
   does not need to be "managed".  It just needs a boot loader.

   This one is built on top of the 3if monitor.

*/

/* This is the same as monitor_3if, except that the internal state of
   the monitor is hidden.  Only the data stack is available. */
#include <stdint.h>
struct script_3if {
    uint8_t *ds;
};

int script(struct script_3if *s) {
    return 0;
}
