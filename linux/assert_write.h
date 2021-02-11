#ifndef ASSERT_WRITE_H
#define ASSERT_WRITE_H

#include "macros.h"

static inline void assert_write(int fd, const uint8_t *buf, size_t len) {
    size_t written = 0;
    while(written < len) {
        ssize_t rv;
        while ((rv = write(fd, buf + written, len - written)) <= 0) {
            /* FIXME: i/o is still non-blocking.  I don't remember why
               this was, because in theory the poll() should ensure we
               never end up in a blocking read. */
            //if (EAGAIN == errno) {
            //    LOG(warning);
            //    sleep();
            //}
            //else {
                ERROR("write(%d,%p,%d) == %d, errno=%d, strerror=\"%s\"\n",
                      fd,buf,len,  rv, errno,    strerror(errno));
            //}
        }
        written += rv;
    }
}




#endif
