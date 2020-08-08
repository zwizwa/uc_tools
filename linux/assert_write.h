#ifndef ASSERT_WRITE_H
#define ASSERT_WRITE_H

#include "macros.h"

static inline void assert_write(int fd, const uint8_t *buf, size_t len) {
    size_t written = 0;
    while(written < len) {
        int rv;
        while ((rv = write(fd, buf, len)) <= 0) {
            /* FIXME: i/o is still non-blocking.  I don't remember why
               this was, because in theory the poll() should ensure we
               never end up in a blocking read. */
            ERROR("write(%d,%p,%d) == %d, errno=%d, strerror=\"%s\"\n",
                         fd,buf,len,  rv, errno,    strerror(errno));
        }
        written += rv;
    }
}




#endif