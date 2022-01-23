/* Misc tools for handling 16-bit be tag protocol. */

#ifndef TAG_PROTOCOL_H
#define TAG_PROTOCOL_H

#include "uct_byteswap.h"
#include "assert_read.h"
#include <stdint.h>

uint32_t assert_read_be(int fd, uint32_t nb) {
    uint8_t buf[nb];
    assert_read(fd, buf, sizeof(buf));
    return read_be(buf, nb);
}

#define LET_NEXT_MSG(msg,len,fd)          \
    uint32_t len = assert_read_be(fd, 4); \
    uint8_t msg[len];                     \
    assert_read(fd, msg, len);

#endif // TAG_PROTOCOL_H
