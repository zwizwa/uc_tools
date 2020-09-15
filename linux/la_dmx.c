/* 8-port DMX sniffer,
   e.g. for use with Saleae Logic8 in data logging mode. */

#include "la_uart.h"
#include "assert_read.h"
#include "assert_write.h"
#include "byteswap.h"


// First data bit is 0, the following are the bit number of parity and stop bits
#define BIT_PARITY -1
// FIXME: la_uart does not check the 2nd stop bit.  For now that is probably ok.
#define BIT_STOP    8

// These should be confiugrable
#define UART_DIV   8   // 2MHz sample rate / 250kBaud UART
#define SAMPLE_MHZ 2

// DMX frame timeout.  For debugging purposes it is probably best to
// keep this low.  Note that DMX standard allows for 1 second before
// calling lost data.
#define MAX_IDLE_MS(ms) (SAMPLE_MHZ * 1000 * (ms))
#define MAX_IDLE MAX_IDLE_MS(100)

// For time stamp logging, use the same resolution as STM32 clock
#define TIME_MUL   (72 / SAMPLE_MHZ)

// Stop bit position is used to encode frame errors and the location
// of the control code tag.  See la_uart.h
#define BRK  (1 << BIT_STOP)
#define IDLE (1 << (BIT_STOP + 1))

/* Convert direct stream on stdin to {packet,4} DMX packet stream on stdout. */
#define BUF_SIZE 1024
struct uart_out {
    struct la la;
    uint16_t buf[BUF_SIZE];
    uintptr_t count;
    la_time_t brk_time;
    uintptr_t port;
};

static void frame_print(struct uart_out *s) {
    uint64_t timestamp = TIME_MUL * (uint64_t)s->brk_time;
    /* About the format: The ':' can be used to separate header from
       payload.  The header contains size, but that is mostly to make
       it human-readable. */
    printf("%08x %d %d:", (uint32_t)timestamp, s->port, s->count);
    for(int i=0; i<s->count; i++) { printf(" %02x", s->buf[i]); }
    printf("\n");
}

#define HDR_SIZE (2+2+4)
static void frame_send(struct uart_out *s) {
    uint8_t buf[4 + HDR_SIZE + BUF_SIZE] = {
        U32_BE(HDR_SIZE + s->count),
        U16_BE(0xC805),
        U16_BE(s->port),
        U32_BE(s->brk_time)
    };
    memcpy(buf + 4 + HDR_SIZE, s->buf, s->count);
    assert_write(1, buf, 4 + HDR_SIZE + s->count);
}

// Make this configurable
static void frame_out(struct uart_out *s) {
    if (1) {
        frame_print(s);
    }
    else {
        frame_send(s);
    }
}

static void uart_out(struct la *la, const struct la_event *e) {
    struct uart_out *s = (void*)la;
    if (e->value == BRK) {
        // Break.  Print collected packet in text log format.
        frame_out(s);
        // Set up for capturing the next packet
        s->count = 0;
        s->brk_time = e->time;
    }
    else if (e->value == IDLE) {
        // Idle.  If there was a non-zero length packet then send it.
        // FIXME: This is not entirely correct.  We do want to report
        // zero length packets (break + no data).
        if (s->count > 0) {
            frame_out(s);
            // Set up for capturing the next packet
            s->count = 0;
        }
    }
    else {
        if (s->count < sizeof(s->buf)) {
            s->buf[s->count++] = e->value;
        }
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
        .config = &c_##n                        \
    };                                          \

#define FOR_PORTS(m) \
    m(0) m(1) m(2) m(3) \
    m(4) m(5) m(6) m(7) \

#define PUSH(n) \
    la_uart_push(&s_##n, &e);

static void start(void) {
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
}
#undef PUSH

int main(int argc, char **argv) {
    LOG("%s\n", argv[0]);
    start();
    return 0;
}
