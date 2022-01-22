#ifndef INFO_BIN_H
#define INFO_BIN_H

/* Binary trace logger, piggy-backed on a text logger.
   Assumes the text logger produces 7-bit clean ASCII.

   We use the bytes 0x80-0xFF as a marker to indicate a time stamped message follows.
   The type stamp is 32 bits host order.
   The low 7 bits in the marker byte contain the number of bytes _after_ the timestamp.

   The point of this is to be fast, since infof() extended with ascii
   timestamping is very slow.

   Encoding should probably be custom to encode high volume messages
   with a small amount of bytes in order not to cause buffer
   overflows.
*/



#include "cycle_counter.h"

#define INFO_BIN_U8(...) {                                      \
        uint8_t msg[] = {__VA_ARGS__};                          \
        info_bin(cycle_counter(), msg, sizeof(msg));            \
    }




#endif
