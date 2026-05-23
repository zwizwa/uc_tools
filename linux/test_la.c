#include "macros.h"

/* Tests for the UART decoder for 8N2 DMX and 9M1 DMX 9 bit extension.
   Generate bit sequence, check what comes out.  FIXME: Just visual
   inspection.  Turn this into an assert over generated tests. */

#include "la_uart.h"
#include "assert_read.h"
#include "assert_write.h"
#include "uct_byteswap.h"

/* Note that DMX is 8N2.

   Up to this 20260523 change I always ignored that second stop bit
   but it is important to just read it out do proper validation of the
   2 stop bits and also suipport the 9N1 extensions.

*/

/* Note that the LA does not use stm token conventions used in embedded code.
   E.g. 0x200 is not break.  It is a control code (IDLE?) */


// First data bit is 0, the following are the bit number of parity and stop bits
#define BIT_PARITY -1
// FIXME: la_uart does not check the 2nd stop bit.  For now that is probably ok.
#define BIT_STOP    9

// These should be configurable
#define UART_DIV   8   // 2MHz sample rate / 250kBaud UART
#ifndef SAMPLE_MHZ
#define SAMPLE_MHZ 2
#endif

/* Control bit is set to indicate that this is not a regular reception
   but a control/state report of the decoder, e.g. line idle
   condition */
#define BIT_CONTROL         (BIT_STOP+1)
#define BIT_CONTROL_MASK    ((1<<BIT_CONTROL)-1)

#define BUF_SIZE 1024

#define MAX_IDLE_MS(ms) (SAMPLE_MHZ * 1000 * (ms))
#define MAX_IDLE MAX_IDLE_MS(100)


struct uart_out {
    struct la la;
    uint16_t buf[BUF_SIZE];
    uintptr_t count;
    la_time_t brk_time;
    uintptr_t port;
};

void uart_out(struct la *la, const struct la_event *e) {
    uint16_t token = e->value;
    // LOG("token 0x%x\n", token);
    if (token & (1 << BIT_CONTROL)) {
        uint16_t control_code = token & BIT_CONTROL_MASK;
        switch(control_code) {
        case 0:
            LOG("idle\n");
            break;
        default:
            LOG("unknown control 0x%x\n", control_code);
            break;
        }
    }
    else if (token & (1 << BIT_STOP)) {
        LOG("frame error 0x%x\n", e->value);
    }
    else {
        LOG("data 0x%x\n", e->value);
    }
}

/* Instantiation macros. */
#define DEF_PORT(n)                             \
    struct uart_out out_##n = {                 \
        .la = { .push = uart_out },             \
        .port = n,                              \
    };                                          \
    struct la_uart_config c_##n = {             \
        .out = &out_##n.la,                     \
        .channel    = n,                        \
        .clock_div  = UART_DIV,                 \
        .bit_stop   = BIT_STOP,                 \
        .bit_parity = BIT_PARITY,               \
        .max_idle   = MAX_IDLE,                 \
    };                                          \
    struct la_uart s_##n = {                    \
        .config = &c_##n,                       \
        .idle_expire = MAX_IDLE,                \
    };                                          \


// #define FOR_PORTS(m) m(0) m(1) m(2) m(3) m(4) m(5) m(6) m(7)

#define FOR_PORTS(m) m(0)

#define PUSH(n) \
    la_uart_push(&s_##n, &e);

struct la_event e = {};
FOR_PORTS(DEF_PORT);

static inline void bit(uint8_t val) {
    e.value = val;
    for (int i=0; i<UART_DIV; i++) {
        FOR_PORTS(PUSH);
        e.time++;
    }
}
static inline void repeat(uint8_t val, int count) {
    for (int i=0; i<count; i++) {
        bit(val);
    }
}

void word(uint8_t b, int bits, int stop) {
    bit(0); // start bit
    for (int i=0; i<bits; i++) {
        bit(b); // data bits
        b >>= 1;
    }
    for (int i=0; i<stop; i++) {
        bit(1); // stop bits
    }
}
void byte(uint8_t b) {
    word(b, 8, 2);
}

void test1(void) {
    repeat(1, 10); // idle
    byte(0x55);
    byte(0xAA);
    byte(0x00);
    byte(0xFF);
    repeat(1, 10); // idle
    repeat(0, 1000); // break
    repeat(1, MAX_IDLE/UART_DIV + 3); // idle
}


int main(int argc, char **argv) {
    LOG("test_la.c\n");
    test1();
}
