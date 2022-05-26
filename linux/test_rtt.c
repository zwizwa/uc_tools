#include "rtt.h"
#include "macros.h"

void test(void) {
    uint8_t rtt_up[100];
    uint8_t rtt_down[100];
    struct rtt_1_1 rtt = RTT_1_1_INIT(rtt_up, rtt_down);
    const char msg[] = "hello\n";
    rtt_target_up_write(&rtt.hdr, 0, (const uint8_t*)msg, strlen(msg));
    rtt_target_up_write(&rtt.hdr, 0, (const uint8_t*)msg, strlen(msg));
    for(;;) {
        uint8_t buf[1];
        uint32_t nb = rtt_host_up_read(&rtt.hdr, 0, buf, 1);
        if (nb == 0) break;
        LOG("%c", buf[0]);
    }
    LOG("\n");
}
int main(int argc, char **argv) {
    test();
    return 0;
}
