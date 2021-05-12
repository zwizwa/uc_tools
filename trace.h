#ifndef TRACE_H
#define TRACE_H

/* Simple trace logger.

   The point of this is to be fast, since infof() extended with ascii
   timestamping is very slow.

   So keep it simple:
   0x80 - 0xFF indicates a binary message follows, payload size is 7 LSBs
   4 bytes timestamp, host order
   n bytes payload
*/


#include "cycle_counter.h"

#define TRACE(...) {                                            \
        uint32_t msg[] = {0, cycle_counter(), __VA_ARGS__};     \
        trace_write(((uint8_t*)msg)+3, sizeof(msg)-3);          \
    }

static inline void trace_write(uint8_t *buf, uint32_t size) {
    buf[0] = (size - 4) | 0x80;
    info_write(buf, size);
}




#endif
