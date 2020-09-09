#include "la_uart.h"
#include "assert_read.h"

// First data bit is 0, the following are the bit number of parity and stop bits
#define BIT_PARITY -1
// FIXME: la_uart does not check the 2nd stop bit.  For now that is probably ok.
#define BIT_STOP    8

// These should be confiugrable
#define UART_DIV  4   // 1MHz sample rate / 250kBaud UART
#define TIME_MUL  72  // STM32 clock rate / 1Mhz sample rate.

#define BRK (1 << BIT_STOP)

/* Convert direct stream on stdin to {packet,4} DMX packet stream on stdout. */
struct uart_out {
    struct la la;
    uint16_t buf[1024];
    uintptr_t count;
    la_time_t brk_time;
};

void uart_out(struct la *la, const struct la_event *e) {
    struct uart_out *s = (void*)la;
    if (e->value == BRK) {
        // Break.  Print collected packet in text log format.
        uint64_t timestamp = TIME_MUL * (uint64_t)s->brk_time;
        printf("%08x", (uint32_t)timestamp);
        for(int i=0; i<s->count; i++) { printf(" %02x", s->buf[i]); }
        printf("\n");
        // Set up for capturing the next packet
        s->count = 0;
        s->brk_time = e->time;
    }
    else {
        if (s->count < sizeof(s->buf)) {
            s->buf[s->count++] = e->value;
        }
    }
}


void start(void) {
    struct uart_out out = {
        .la = { .push = uart_out },
    };
    struct la_uart_config c = {
        .out = &out.la,
        .channel = 0,
        .clock_div  = UART_DIV,
        .bit_stop   = BIT_STOP,
        .bit_parity = BIT_PARITY,
    };
    struct la_uart s = {
        .config = &c
    };

    uint8_t in_buf[256 * 1024]; // Logic8 write size.
    for(;;) {
        struct la_event e = {};
        ssize_t n = assert_read(0, in_buf, sizeof(in_buf));
        for(size_t i=0; i<n; i++) {
            e.value = in_buf[i];
            la_uart_push(&s, &e);
            e.time++;
        }
    }
}
int main(int argc, char **argv) {
    LOG("%s\n", argv[0]);
    start();
    return 0;
}
