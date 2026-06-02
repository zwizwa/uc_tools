#include "userfault.h"

int main(int argc, char **argv) {
    LOG("test_userfaultfd.c\n");
    // Create the memory with handler.
    struct userfault s = {};
    userfault_init(
        &s,
        1<<30,
        userfault_example_service,
        &s);

    // First read creates a page fault and handler should run.
    uint32_t ps = page_size();
    int offset[] = {
        0x123,
        0x124,
        ps + 1,
        ps + 2,
    };
    for (int j=0; j<ARRAY_SIZE(offset); j++) {
        int i = offset[j];
        LOG("mem[0x%x] = 0x%x\n", i, s.mem[i]);
    }
    LOG("done\n");
    sleep(1);
    // FIXME: the read will crash
}
