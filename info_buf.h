#ifndef INFO_BUF_H
#define INFO_BUF_H

#include <stdint.h>

/* Structure is specified, to allow access via external debugger.  The
   sizes here are just defaults.  External access needs to check
   .logsize field. */

/* Note that in the old implementation, INFO_LOGSIZE was a constant.
   This is too hard to make work in generic code if we quickly want to
   change the size of the log buffer in one place, so now it points to
   the struct member. */
//#define INFO_LOGSIZE 10
/* This is a header.  The buffer follows. */
struct info_buf_hdr {
    uint16_t write_next;
    uint16_t read_next;
    uint8_t  logsize;
    uint8_t  reserved[3];
};



#endif
