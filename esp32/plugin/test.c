#include "../main/iot_bios.h"
// The code is executed with 3if JSR which is passed the monitor_esp
// struct, which is what we implement here as struct overlay that
// hides things we don't need.
struct state {
    void *_3if_priv[16];
    const struct iot_bios *iot_bios;
};

void run(struct state *s) {
    s->iot_bios->printf("test.c run at %p\n", &run);
    // s->iot_bios->log_u32(0x55555555);
}

// I ran into "l32r: literal placed after use" when the function at
// the start of iram refers to literals, so just use a trampoline that
// doesn't use any literals.
__attribute__((section(".run")))
__attribute__((__noinline__))
void _run(struct state *s) {
    run(s);
}

