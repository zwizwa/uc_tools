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

#endif // TAG_PROTOCOL_H
