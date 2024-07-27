#include "../main/esp_os.h"
// The code is executed with 3if JSR which is passed the monitor_esp
// struct, which is what we implement here as struct overlay that
// hides things we don't need.
struct state {
    void *_priv[9];
    const struct esp_os *esp_os;
};

__attribute__((section(".run")))
__attribute__((__noinline__))
void run(struct state *s) {
    s->esp_os->printf("test.c run at %p\n", &run);
    // s->esp_os->log_u32(0x55555555);
}

