typedef int size_t;
#include "../../iot_bios.h"
// The code is executed with 3if JSR which is passed the monitor_esp
// struct, which is what we implement here as struct overlay that
// hides things we don't need.
struct state {
    void *_priv[9];
    const struct iot_bios *iot_bios;
};

__attribute__((section(".run")))
__attribute__((__noinline__))
void run(struct state *s) {
    s->iot_bios->printf("test.c run at %p\n", &run);
    // s->iot_bios->log_u32(0x55555555);
}

