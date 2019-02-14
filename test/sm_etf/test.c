/* Test harness for sm_etf that doesn't need to run on a uC */

#include "base.h"
#include "sm_etf.h"

struct sm_etf sm_etf;

void test(const uint8_t *buf, uint32_t len) {
    for (int i=0; i<len; i++) {
        infof("%02x ", buf[i]);
    }
    infof("\n");

    uint32_t status = sm_etf_write(&sm_etf, buf, len);
    infof("status: %08x\n", status);
}
#define TEST(init) { uint8_t buf[] = init; test(buf, sizeof(buf)); }

int main(int argc, char **argv) {
    infof("%s\n", argv[0]);
    sm_etf_init(&sm_etf);
    TEST({131});
    sm_etf_init(&sm_etf);
    TEST({45});
}
