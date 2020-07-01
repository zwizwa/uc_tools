// killall klogd, then run this to log kernel events to console
// fixme: listen for stdin close

#include <sys/klog.h>
#include <unistd.h>
#include <stdint.h>
#include <pthread.h>
#include <stdlib.h>

// FIXME: read from /proc/kmsg instead?
int main(void) {
    system("killall klogd");
    for(;;) {
        uint8_t buf[1024];
        ssize_t len = klogctl(2, (void*)buf, sizeof(buf));
        write(1, buf, len);
    }
    return 0;
}
