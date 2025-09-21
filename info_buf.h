#ifndef INFO_BUF_H
#define INFO_BUF_H

#include "macros.h"
#include <stdint.h>

/* The header structure is specified as API, to allow access via
   external debugger.

   The .write_next and .read_next indices are rolling.  To get the
   index into the buffer, mask out the lower .logsize bits.

   Note that we only use this on ARMv7-M, where 32bit read/write is
   atomic.

   See A3.5.3 Atomicity in the ARM architecture, ARMv7-M_ARM.pdf
*/

/* Buffer follows the header. */
struct info_buf_hdr {
    uint32_t write_next;
    uint32_t read_next;
    uint8_t  logsize;
    uint8_t  level_threshold; // (1)
    uint8_t  level_current; // (1)
    uint8_t  keep_old:1;
    uint8_t  use_spillover:1;
};

/* (1)

   Zero means default behavior: log everything.

   This is a quick hack to implement log levels.  Two variables are
   used.  'threshold' sets the global log level, which gates
   info_putchar_inner.  'current' sets the threshold of the current
   logging context, which is modified by statements that support
   level-based logging, and is put back to 0 after the extent of the
   log call.

   Practically, too much needs to change to implement the threshold as
   context that is passed down the calls. The info mechanism was
   already non-reentrant, so this seems ok as a temporary hack.
   FIXME: Evaluate this later.
*/


CT_ASSERT(info_buf_hdr_size,sizeof(struct info_buf_hdr)==12);


#endif
