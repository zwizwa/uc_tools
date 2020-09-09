#include "la_uart.h"

void uart_out(struct la *la, const struct la_event *e) {
    LOG("%d\n", e->value);
}

// First data bit is 0, the following are the bit number of parity and stop bits
#define BIT_PARITY 8
#define BIT_STOP   9
#define UART_DIV   4

static inline void test_uart_bits(struct la_uart *s, struct la_event *e, int value, int nb_bits) {
    for (int j=0; j<nb_bits; j++) {
        e->value = (value >> j) & 1;
        for (int i=0; i<s->config->clock_div; i++) {
            la_uart_push(s, e);
            e->time++;
        }
    }
}
static inline void test_uart_bytes(struct la_uart *s, struct la_event *e, const uint8_t *b, int nb_bytes) {
    for (int i=0; i<nb_bytes; i++) {
        test_uart_bits(s, e, -1,   1); // idle
        test_uart_bits(s, e, 0,    1); // start
        test_uart_bits(s, e, b[i], 8); // stop
        test_uart_bits(s, e, -1,   2); // stop
    }
}
void test_uart(void) {
    struct la out = {
        .push = uart_out
    };
    struct la_uart_config c = {
        .out = &out,
        .channel = 0,
        .clock_div  = UART_DIV,
        .bit_stop   = BIT_STOP,
        .bit_parity = BIT_PARITY,
    };
    struct la_uart s = {
        .config = &c
    };
    uint8_t test_data[] = {1,2,3};
    struct la_event e = {};
    test_uart_bytes(&s, &e, test_data, sizeof(test_data));
}

int main(int argc, char **argv) {
    LOG("%s\n", argv[0]);
    test_uart();
    return 0;
}
