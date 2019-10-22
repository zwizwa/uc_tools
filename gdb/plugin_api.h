/* FIXME: This needs work.  Pure code is not enough.  State is needes as well.
   .after loading, an init routine is needed to reset state
   .data needs to be copied
   .bss  needs to be initialized
*/


#ifndef PLUGIN_API_H
#define PLUGIN_API_H

#include "gdbstub_api.h"

/* A second level of 'plugin/ seems to be necessary:
   1. bootloader   minimal, API+protocol changes infrequently
   2. application  changes frequently, needs power cycling (due to USB resistors)
   3. plugin       faster reload without restart + can use code in application

   Note that the need for 3 is purely a practical one.  Think of it as
   a "generational" solution that loads faster, doesn't require a
   reboot, and can use a larger support system, while the gdbstub
   loader needs to be small and needs to speak raw GDB RSP.

.  Once code is stable, it can be moved from 3 to 2.  Maybe
   "scratchpad" is a better name for 3.

*/


/* To abstract a plugin, use a binary stream read/write interface.
   This obeys the same rules as the read/write interface to a normal
   application. */

#define PLUGIN_API_VERSION 0x00014C50  // Version + 'PL' tag
struct plugin_service {
    uint32_t version;       // Maybe best to use some version/tag info
    struct gdbstub_io io;   // Plugin's input/output.  Sending a message will activate.
    gdbstub_fn_start start; // Needs to be called once before io is accessed.
    gdbstub_fn_stop  stop;  // If nonzero, needs to be called before code is reloaded.
};

// FIXME: Some tag to identify which host this was compiled for.


/* Flash-based plugins.  Header is top of Flash segment of the plugin
   code, which is bottom (_eflash) of the host image. */
#define PLUGIN_HEADER_SECTION __attribute__ ((section (".plugin_header")))

#endif
