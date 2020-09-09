#include "la_uart.h"

/* Test for lu_uart.h using DMX framing. */

struct uart_out {
    struct la la;
    uint16_t buf[1024];
    uintptr_t count;
};

void uart_out(struct la *la, const struct la_event *e) {
    struct uart_out *s = (void*)la;
    s->buf[s->count++] = e->value;
}

// First data bit is 0, the following are the bit number of parity and stop bits
#define BIT_PARITY -1 // no parity
#define BIT_STOP    8
#define UART_DIV    4

#define BRK (1 << BIT_STOP)

static inline void test_uart_bits(struct la_uart *s, struct la_event *e, int value, int nb_bits) {
    for (int j=0; j<nb_bits; j++) {
        e->value = value & 1;
        value >>= 1;
        for (int i=0; i<s->config->clock_div; i++) {
            la_uart_push(s, e);
            e->time++;
        }
    }
}
static inline void test_uart_bytes(struct la_uart *s, struct la_event *e, const uint16_t *b, int nb_bytes) {
    for (int i=0; i<nb_bytes; i++) {
        if (b[i] == BRK) {
            // DMX Break=176uS, MAB=44
            // At 250kBaud, a bit is 4uS
            int bit_us = 4;
            int brk_ticks = (UART_DIV * 176) / bit_us;
            int mab_ticks = (UART_DIV *  44) / bit_us;

            test_uart_bits(s, e,  0, brk_ticks);
            test_uart_bits(s, e, -1, mab_ticks);
        }
        else if (b[i] < 256) {
            test_uart_bits(s, e, -1,   1); // idle
            test_uart_bits(s, e, 0,    1); // start
            test_uart_bits(s, e, b[i], 8); // data
            test_uart_bits(s, e, -1,   2); // stop
        }
        else {
            LOG("unknown token: %d\n", b[i]);
        }
    }
}
void test_uart_assert(uint16_t *test_data, uintptr_t test_size) {
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
    struct la_event e = {};
    test_uart_bytes(&s, &e, test_data, test_size);
    for(int i=0; i<out.count; i++){
        LOG("%d ", out.buf[i]);
    }
    LOG("\n");
    ASSERT(test_size == out.count);
    for(int i=0; i<out.count; i++){
        ASSERT(test_data[i] == out.buf[i]);
    }
}
void test_uart(void) {
    uint16_t test_data[] = {BRK, 1, 2, 3, 4};
    test_uart_assert(test_data, 5);
}

int main(int argc, char **argv) {
    LOG("%s\n", argv[0]);
    test_uart();
    return 0;
}
