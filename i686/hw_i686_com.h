#ifndef HW_I686_COM
#define HW_I686_COM

#include "hw_i686_io.h"

// Serial port
// https://claude.ai/chat/c5b674e0-3495-485e-ad01-2746fbd8b669

/* COM1 I/O ports */
#define COM1_BASE   0x3F8
#define COM1_DATA   (COM1_BASE + 0)  /* RBR: Receive Buffer Register */
#define COM1_IER    (COM1_BASE + 1)  /* Interrupt Enable Register */
#define COM1_FCR    (COM1_BASE + 2)  /* FIFO Control Register */
#define COM1_LCR    (COM1_BASE + 3)  /* Line Control Register */
#define COM1_MCR    (COM1_BASE + 4)  /* Modem Control Register */
#define COM1_LSR    (COM1_BASE + 5)  /* Line Status Register */

/* LSR bits */
#define LSR_DATA_READY   0x01
#define LSR_OVERRUN_ERR  0x02
#define LSR_PARITY_ERR   0x04
#define LSR_FRAMING_ERR  0x08
#define LSR_THR_EMPTY    0x20  /* Transmitter Holding Register empty */
#define LSR_TX_IDLE      0x40  /* Transmitter fully idle (THR + shift reg) */

static void com1_write_byte(uint8_t byte) {
    while (!(inb(COM1_LSR) & LSR_THR_EMPTY)) { }
    outb(COM1_DATA, byte);
}
static void com1_putchar(uint8_t byte) {
    int raw = 0;  // FIXME: put all com config in a struct
    if (!raw && (byte == '\n')) {
        com1_write_byte('\r');
    }
    com1_write_byte(byte);
}
static void com1_putstr(uint8_t *str) {
    while(*str) {
        com1_putchar(*str++);
    }
}
static void com1_init(void) {
    /* DTR, RTS, OUT2 (OUT2 gates IRQs on PCs) */
    outb(COM1_MCR, 0x0B);
    /* enable "Received Data Available" interrupt */
    outb(COM1_IER, 0x01);
}



#endif
