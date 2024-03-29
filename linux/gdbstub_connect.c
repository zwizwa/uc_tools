#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <fcntl.h>
#include <sys/ioctl.h>
#include <poll.h>
#include <unistd.h>
#include "raw_serial.h"

#include "macros.h"

static void transfer(int dst_fd, int src_fd) {
    uint8_t buf[4096]; // FIXME: Size?
    int n = read(src_fd, buf, sizeof(buf));
    ASSERT(n > 0);
    ASSERT(n == write(dst_fd, buf, n));
}

static int bridge(const char *devname) {

    LOG("devname = %s\n", devname);

    int ser_fd;
    ASSERT_ERRNO(ser_fd = open(devname, O_RDWR | O_NONBLOCK));

    raw_serial_config(ser_fd);

    struct pollfd pfd[2] = {
        [0] = { .events = POLLIN, .fd = 0  },
        [1] = { .events = POLLIN, .fd = ser_fd }
    };
    for(;;) {
        int rv;
        ASSERT_ERRNO(rv = poll(&pfd[0], 2, -1));
        ASSERT(rv >= 0);

        /* stdin */
        if(pfd[0].revents & POLLIN) {
            transfer(ser_fd, 0);
        }
        else {
            ASSERT(0 == pfd[0].revents);
        }
        /* serial */
        if(pfd[1].revents & POLLIN) {
            transfer(1, ser_fd);
        }
        else {
            ASSERT(1 == pfd[0].revents);
        }
    }
}
int main(int argc, char **argv) {

    // FIXME: Assert that devname contains a valid device name.
    // Command injection is possible.

    const char *devname;
    // Direct invocation
    if ((argc == 2) && (devname = argv[1])) {
        return bridge(devname);
    }
    // Run through authorized_keys COMMAND= mechanism, which allows
    // fine-grained access control to perform update of port binaries.
    // Note that at the ssh end, the command name is dropped as it
    // will be overridden anyway.  The original command contains just
    // the argument.
    if ((devname = getenv("SSH_ORIGINAL_COMMAND"))) {
        return bridge(devname);
    }
    ASSERT(devname);


}
