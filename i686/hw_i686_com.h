#ifndef HW_I686_COM
#define HW_I686_COM

#include "hw_i686_io.h"

// Serial port
// https://claude.ai/chat/c5b674e0-3495-485e-ad01-2746fbd8b669

/* COM1 I/O ports */
#define UART_DATA   0  /* RBR: Receive Buffer Register */
#define UART_IER    1  /* Interrupt Enable Register */
#define UART_FCR    2  /* FIFO Control Register */
#define UART_IIR    2  /* Interrupt Identification Register, read */
#define UART_LCR    3  /* Line Control Register */
#define UART_MCR    4  /* Modem Control Register */
#define UART_LSR    5  /* Line Status Register */
#define UART_SCR    7  /* Scratch Register */

/* IER bits */
#define IER_RDA     0x01  /* Received Data Available */
#define IER_THRE    0x02  /* Transmitter Holding Register Empty */
#define IIR_NONE    0x01  /* no interrupt pending */

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
    volatile uint8_t *mmio;  // instead of iobase
};
static inline uint8_t uart_in(struct uart *s, uint8_t reg) {
    return s->mmio ? s->mmio[reg] : inb(s->iobase + reg);
}
static inline void uart_out(struct uart *s, uint8_t reg, uint8_t val) {
    if (s->mmio) s->mmio[reg] = val;
    else outb(s->iobase + reg, val);
}

static void uart_write_byte(struct uart *s, uint8_t byte) {
    while (!(uart_in(s, UART_LSR) & LSR_THR_EMPTY)) { }
    uart_out(s, UART_DATA, byte);
}
static void uart_putchar(struct uart *s, uint8_t byte) {
    int raw = 0;  // FIXME: put all com config in a struct
    if (!raw && (byte == '\n')) {
        uart_write_byte(s, '\r');
    }
    uart_write_byte(s, byte);
}
static void uart_putstr(struct uart *s, char *str) {
    while(*str) {
        uart_putchar(s, *str++);
    }
}

/* Interrupt driven tx: one producer, drained by uart_tx_isr.  The
   producer publishes, then enables THRE, so no wakeup is lost.  THRE
   is toggled for a fresh edge: the PC pic is edge triggered and an
   edge can be lost, e.g. before the pic is set up at boot. */
#define UART_TX_SIZE 4096  // power of two
#define UART_FIFO 16
struct uart_tx {
    uint8_t buf[UART_TX_SIZE];
    volatile uint32_t write, read;  // free running
};
static inline int uart_tx_full(struct uart_tx *t) {
    return t->write - t->read == UART_TX_SIZE;
}
/* Returns -1 when full. */
static inline int uart_tx_put(struct uart *s, struct uart_tx *t, uint8_t byte) {
    if (uart_tx_full(t)) return -1;
    t->buf[t->write % UART_TX_SIZE] = byte;
    __asm__ volatile ("" ::: "memory");
    t->write++;
    uart_out(s, UART_IER, IER_RDA);
    uart_out(s, UART_IER, IER_RDA | IER_THRE);
    return 0;
}
/* Isr, or poll with interrupts off. */
static inline void uart_tx_isr(struct uart *s, struct uart_tx *t) {
    if (!(uart_in(s, UART_LSR) & LSR_THR_EMPTY)) return;
    for (int i = 0; i < UART_FIFO && t->read != t->write; i++) {
        uart_out(s, UART_DATA, t->buf[t->read % UART_TX_SIZE]);
        t->read++;
    }
    if (t->read == t->write) uart_out(s, UART_IER, IER_RDA);
}
/* 16550 present: scratch register keeps its value. */
static inline int uart_present(struct uart *s) {
    uart_out(s, UART_SCR, 0x5A);
    if (uart_in(s, UART_SCR) != 0x5A) return 0;
    uart_out(s, UART_SCR, 0xA5);
    return uart_in(s, UART_SCR) == 0xA5;
}

typedef void (*uart_sink_fn)(void *, uint8_t);
static void uart_isr(struct uart *s, uart_sink_fn sink, void *ctx) {
    uint8_t lsr;
    while ((lsr = uart_in(s, UART_LSR)) & LSR_DATA_READY) {
        uint8_t byte = uart_in(s, UART_DATA);
        /* Optionally inspect lsr for framing/parity/overrun errors */
        if (lsr & (LSR_OVERRUN_ERR | LSR_PARITY_ERR | LSR_FRAMING_ERR)) {
            /* drop or log — byte is still worth passing up in most designs */
        }
        sink(ctx, byte);
    }
}

static void uart_init(struct uart *s, uint16_t div) {

    /* divisor:
       1 -> 115200
       3 -> 38400
       6 -> 19200 */

    /* disable interrupts while configuring */
    uart_out(s, UART_IER,  0x00);

    uart_out(s, UART_LCR,  0x80);   /* DLAB = 1, access divisor latch */
    uart_out(s, UART_DATA, div & 0xFF); /* divisor low */
    uart_out(s, UART_IER,  div >> 8);   /* divisor high */

    uart_out(s, UART_LCR,  0x03);   /* DLAB = 0, 8 bits, no parity, 1 stop */
    uart_out(s, UART_FCR,  0xC7);   /* enable FIFO, clear RX/TX, 14-byte trigger */

    /* DTR, RTS, OUT2 (OUT2 gates IRQs on PCs) */
    uart_out(s, UART_MCR,  0x0B);
    /* enable "Received Data Available" interrupt */
    uart_out(s, UART_IER,  0x01);
}


#endif
