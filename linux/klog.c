// killall klogd, then run this to log kernel events to console

#include <sys/klog.h>
#include <unistd.h>
#include <stdint.h>

int main(void) {
    for(;;) {
        uint8_t buf[1024];
        ssize_t len = klogctl(2, (void*)buf, sizeof(buf));
        write(1, buf, len);
    }
    return 0;
}
