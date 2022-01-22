#ifndef INFO_BIN_H
#define INFO_BIN_H

/* Binary trace logger.

   The point of this is to be fast, since infof() extended with ascii
   timestamping is very slow.

   So keep it simple:
   0x80 - 0xFF indicates a binary message follows, payload size post timestamp is 7 LSBs
   4 bytes timestamp, host order
   n bytes payload

*/



#include "cycle_counter.h"

#define INFO_BIN_U32(...) {                                     \
        uint32_t msg[] = {0, cycle_counter(), __VA_ARGS__};     \
        info_bin_write(((uint8_t*)msg)+3, sizeof(msg)-3);       \
    }
static inline void info_bin_write(uint8_t *buf, uint32_t size) {
    buf[0] = (size - 5) | 0x80;
    info_write(buf, size);
}




#endif