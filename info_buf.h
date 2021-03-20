#ifndef INFO_BUF_H
#define INFO_BUF_H

#include <stdint.h>

/* Structure is specified, to allow access via external debugger.  The
   sizes here are just defaults.  External access needs to check
   .logsize field. */

#ifndef INFO_LOGSIZE
#define INFO_LOGSIZE 10
#endif

#define INFO_SIZE (1<<INFO_LOGSIZE)
#define INFO_MASK ((1 << INFO_LOGSIZE)-1)

struct info_buf {
    uint16_t write_next;
    uint16_t read_next;
    uint8_t  logsize;
    uint8_t  reserved[3];
    uint8_t  buf[INFO_SIZE];
};

extern struct info_buf info_buf;

#endif
