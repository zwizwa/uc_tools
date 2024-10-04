
// FIXME: These definitions need to be made abstract.
typedef int size_t;
typedef int esp_ota_handle_t;
typedef int esp_err_t;
typedef int esp_partition_t;

#include "../../bios.h"
// The code is executed with 3if JSR which is passed the monitor_esp
// struct, which is what we implement here as struct overlay that
// hides things we don't need.
struct state {
    void *_3if_priv[16];
    const struct bios *bios;
};

void run(struct state *s) {
    const struct bios *b = s->bios;
    b->printf("%08x test.c run at %p\n", b->cycle_counter(), &run);
}

// I ran into "l32r: literal placed after use" when the function at
// the start of iram refers to literals, so just use a trampoline that
// doesn't use any literals.
__attribute__((section(".run")))
__attribute__((__noinline__))
void _run(struct state *s) {
    run(s);
}

