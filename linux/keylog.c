/*
    Kernel Documentation:
        https://www.kernel.org/doc/html/latest/input/input.html
        https://www.kernel.org/doc/html/latest/input/event-codes.html
    Event codes in kernel source:
        https://github.com/torvalds/linux/blob/master/include/uapi/linux/input-event-codes.h



*/

#include "macros.h"
#include "assert_read.h"
#include <linux/input-event-codes.h>
#include <stdint.h>
struct input_event {
    struct timeval time;
    uint16_t type;
    uint16_t code;
    int32_t value;
};
int main(int argc, char **argv) {
    ASSERT(argc > 1);
    const char *event = argv[1];
    int fd;
    ASSERT_ERRNO(fd = open(event, O_RDONLY));
    for(;;) {
        struct input_event e = {};
        assert_read_fixed(fd, &e, sizeof(e));
        switch (e.type) {
        case EV_SYN: // 0
            break;
        case EV_KEY: // 1
            LOG("EV_KEY code=%d value=%d\n", e.code, e.value);
            break;
        case EV_MSC: // 4
            break;
        default:
            LOG("type=%d code=%d value=%d\n", e.type, e.code, e.value);
        }
    }
}
