#ifndef ASSERT_WRITE_H
#define ASSERT_WRITE_H

#include "macros.h"
#include <stdint.h>
#include <unistd.h>
#include <time.h>

static inline void assert_write(int fd, const uint8_t *buf, size_t len) {
    size_t written = 0;
    while(written < len) {
        ssize_t rv;
        while ((rv = write(fd, buf + written, len - written)) <= 0) {
            ERROR("assert_write: write(%d,%p,%d) == %d, errno=%d, strerror=\"%s\"\n",
                  fd, buf, (int)len, (int)rv, (int)errno, strerror(errno));
        }
        written += rv;
    }
}


/* For non-blocking devices, retry on EAGAIN after sleep. */
static inline void assert_write_nb(int fd, const uint8_t *buf, size_t len, int sleep_ns) {
    size_t written = 0;
    while(written < len) {
        ssize_t rv;
        while ((rv = write(fd, buf + written, len - written)) <= 0) {
            if (EAGAIN == errno) {
                // LOG(warning);
                struct timespec ts = {
                    .tv_sec = 0,
                    .tv_nsec = sleep_ns
                };
                nanosleep(&ts, NULL);
            }
            else {
                ERROR("assert_write: write(%d,%p,%d) == %d, errno=%d, strerror=\"%s\"\n",
                      fd, buf, (int)len, (int)rv, (int)errno, strerror(errno));
            }
        }
        written += rv;
    }
}


#define ASSERT_WRITE(fd, ...) {              \
    const uint8_t buf[] = { __VA_ARGS__ };   \
    assert_write(fd, buf, sizeof(buf));      \
    }


#endif
