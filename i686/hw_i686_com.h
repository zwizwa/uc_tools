#ifndef HW_I686_COM
#define HW_I686_COM

#include "hw_i686_io.h"

// Serial port
// https://claude.ai/chat/c5b674e0-3495-485e-ad01-2746fbd8b669

/* COM1 I/O ports */
#define UART_DATA   0  /* RBR: Receive Buffer Register */
#define UART_IER    1  /* Interrupt Enable Register */
#define UART_FCR    2  /* FIFO Control Register */
#define UART_LCR    3  /* Line Control Register */
#define UART_MCR    4  /* Modem Control Register */
#define UART_LSR    5  /* Line Status Register */

/* LSR bits */
#define LSR_DATA_READY   0x01
#define LSR_OVERRUN_ERR  0x02
#define LSR_PARITY_ERR   0x04
#define LSR_FRAMING_ERR  0x08
#define LSR_THR_EMPTY    0x20  /* Transmitter Holding Register empty */
#define LSR_TX_IDLE      0x40  /* Transmitter fully idle (THR + shift reg) */

/* On PC, uart is assumed to be 16550 */
struct uart {
    uint16_t iobase;
    uint8_t  irq;
};

static void uart_write_byte(struct uart *s, uint8_t byte) {
    while (!(inb(s->iobase + UART_LSR) & LSR_THR_EMPTY)) { }
    outb(s->iobase + UART_DATA, byte);
}
static void uart_putchar(struct uart *s, uint8_t byte) {
    int raw = 0;  // FIXME: put all com config in a struct
    if (!raw && (byte == '\n')) {
        uart_write_byte(s, '\r');
    }
    uart_write_byte(s, byte);
}
static void uart_putstr(struct uart *s, uint8_t *str) {
    while(*str) {
        uart_putchar(s, *str++);
    }
}

typedef void (*uart_sink_fn)(void *, uint8_t);
static void uart_isr(struct uart *s, uart_sink_fn sink, void *ctx) {
    uint8_t lsr;
    while ((lsr = inb(s->iobase + UART_LSR)) & LSR_DATA_READY) {
        uint8_t byte = inb(s->iobase + UART_DATA);
        /* Optionally inspect lsr for framing/parity/overrun errors */
        if (lsr & (LSR_OVERRUN_ERR | LSR_PARITY_ERR | LSR_FRAMING_ERR)) {
            /* drop or log — byte is still worth passing up in most designs */
        }
        sink(ctx, byte);
    }
}

static void uart_init(struct uart *s) {

    /* divisor:
       1 -> 115200
       3 -> 38400
       6 -> 19200 */

    /* disable interrupts while configuring */
    outb(s->iobase + UART_IER,  0x00);

    outb(s->iobase + UART_LCR,  0x80);   /* DLAB = 1, access divisor latch */
    outb(s->iobase + UART_DATA, 0x01);   /* divisor low */
    outb(s->iobase + UART_IER,  0x00);   /* divisor high */

    outb(s->iobase + UART_LCR,  0x03);   /* DLAB = 0, 8 bits, no parity, 1 stop */
    outb(s->iobase + UART_FCR,  0xC7);   /* enable FIFO, clear RX/TX, 14-byte trigger */

    /* DTR, RTS, OUT2 (OUT2 gates IRQs on PCs) */
    outb(s->iobase + UART_MCR,  0x0B);
    /* enable "Received Data Available" interrupt */
    outb(s->iobase + UART_IER,  0x01);
}

/* OLD */
struct uart com1 = {
    .iobase = 0x3F8,
    .irq    = 4,
};
static void com1_write_byte(uint8_t byte) {
    uart_write_byte(&com1, byte);
}
static void com1_putchar(uint8_t byte) {
    uart_putchar(&com1, byte);
}
static void com1_putstr(uint8_t *str) {
    uart_putstr(&com1, str);
}
static void com1_init(void) {
    uart_init(&com1);
}



#endif
