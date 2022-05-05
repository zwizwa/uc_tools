/* 8-port DMX sniffer,
   e.g. for use with Saleae Logic8 in data logging mode. */

struct uart_out;
static void frame_out(struct uart_out *s);

#include "mod_la_dmx.c"

#define FOR_PORTS(m) \
    m(0) m(1) m(2) m(3) \
    m(4) m(5) m(6) m(7) \

#define PUSH(n) \
    la_uart_push(&s_##n, &e);

// Make this configurable
static void frame_out(struct uart_out *s) {
    if (1) {
        frame_print(s);
    }
    else {
        uint16_t tag = 0x1234; // arbitrary
        frame_send(s, tag);
    }
}

int main(int argc, char **argv) {
    LOG("%s\n", argv[0]);

    FOR_PORTS(DEF_PORT)

    uint8_t in_buf[256 * 1024]; // Logic8 write size.
    struct la_event e = {};
    for(;;) {
        ssize_t n = assert_read(0, in_buf, sizeof(in_buf));
        for(size_t i=0; i<n; i++) {
            e.value = in_buf[i];
            FOR_PORTS(PUSH)
            e.time++;
        }
    }

    return 0;
}

