#ifndef ASSERT_READ_H
#define ASSERT_READ_H

#include "macros.h"
#include <unistd.h>

static inline ssize_t assert_read(int fd, void *vbuf, size_t nb) {
    unsigned char *buf = vbuf;
    //LOG("assert_read(%d,%p,%d)\n", fd, buf, nb);
    if (nb == 0) return 0;
    ssize_t rv;
    do {
        rv = read(fd, buf, nb);
    } while(rv == -1 && errno == EINTR); // Haskell uses signals

    if (rv > 0) {
        //LOG("%2d: rv=%d\n", fd, rv);
    }
    else if (rv == 0) {
        LOG("fd %2d: EOF\n", fd);
        exit(0);
    }
    else if (rv < 0) {
        int e = errno;
        LOG("fd %2d: errno=%d\n", fd, e);
        //char *msg = strerror(e);
        //LOG("%d: errno=%d: %s\n", fd, e, msg); // SIGSEGV?
    }
    ASSERT(rv > 0);
    return rv;
}
static inline ssize_t assert_read_fixed(int fd, void *vbuf, size_t nb) {
    unsigned char *buf = vbuf;
    size_t got = 0;
    while (got < nb) {
        ssize_t rv = assert_read(fd, buf+got, nb-got);
        ASSERT(rv > 0);
        got += rv;
        // LOG("got=%d, rv=%d, nb=%d\n", got, rv, nb);
    }
    ASSERT_EQ(got, nb);
    return got;
}


#endif
