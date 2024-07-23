#include "../main/esp_os.h"
// The code is executed with 3if JSR which is passed the monitor_esp
// struct, which is what we implement here as struct overlay that
// hides things we don't need.
struct state {
    void *_priv[9];
    const struct esp_os *esp_os;
};

void run(struct state *s) {
    // Literal strings don't work yet.  They are still loaded into
    // IRAM which only supports 32 bit access.  The image needs to be
    // split into two parts: IRAM and DRAM to make this work, so work
    // around it for now.

    s->esp_os->printf("hello from test.c\n");
    s->esp_os->log_u32(0x55555555);
}

// I ran into "l32r: literal placed after use" when putting run()
// first, so use this trampoline.  The l32r still seems to refer to
// data stored after the reference though, so I don't think I
// understand.
__attribute__((section(".run")))
__attribute__((__noinline__))
void _run(struct state *s) {
    run(s);
}

