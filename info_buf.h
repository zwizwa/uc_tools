#ifndef INFO_BUF_H
#define INFO_BUF_H

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
    uint8_t  reserved[3];
};



#endif
