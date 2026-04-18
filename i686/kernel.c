#include <stdint.h>
#include "text_console.h"

extern uint8_t __bss_start;
extern uint8_t __bss_end;


// Put everything in a single static struct.
struct app {
    struct text_console log;
    struct idt idt;
};

struct app app;

#define KBD_SHIFT  0x80
#define KBD_CTRL   0x81
#define KBD_ALT    0x82
#define KBD_CAPS   0x83

const uint8_t kbd_US[128] = {
    0,  27, '1', '2', '3', '4', '5', '6', '7', '8', '9', '0', '-', '=', '\b',
    '\t', /* <-- Tab */
    'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p', '[', ']', '\n',
    KBD_CTRL, /* <-- control key */
    'a', 's', 'd', 'f', 'g', 'h', 'j', 'k', 'l', ';', '\'', '`',  8,
    '\\', 'z', 'x', 'c', 'v', 'b', 'n', 'm', ',', '.', '/',  KBD_SHIFT,
    '*',
    KBD_ALT,  /* Alt */
    ' ',  /* Space bar */
    KBD_CAPS,  /* Caps lock */
    0,  /* 59 - F1 key ... > */
    0,   0,   0,   0,   0,   0,   0,   0,
    0,  /* < ... F10 */
    0,  /* 69 - Num lock*/
    0,  /* Scroll Lock */
    0,  /* Home key */
    0,  /* Up Arrow */
    0,  /* Page Up */
    '-',
    0,  /* Left Arrow */
    0,
    0,  /* Right Arrow */
    '+',
    0,  /* 79 - End key*/
    0,  /* Down Arrow */
    0,  /* Page Down */
    0,  /* Insert Key */
    0,  /* Delete Key */
    0,   0,   0,
    0,  /* F11 Key */
    0,  /* F12 Key */
    0,  /* All other keys are undefined */
};


__attribute__((naked))
static void keyboard_isr(void) {
    isr_begin();
    uint8_t scancode = inb(0x60);
    uint8_t ascii = kbd_US[scancode & 0x7F];
    // app.log.video[0] = ascii;
    if (!(scancode & 0x80)) {
        // press
        // FIXME: locking / buffering?
        text_console_putchar(&app.log, ascii);
    }
    else {
        // release
    }

    outb(0x20, 0x20); // End Of Interrupt (EOI) to master PIC
    isr_end();
}

__attribute__((naked))
static void com1_isr(void) {
    isr_begin();
    uint8_t lsr;
    while ((lsr = inb(COM1_LSR)) & LSR_DATA_READY) {
        uint8_t byte = inb(COM1_DATA);

        /* Optionally inspect lsr for framing/parity/overrun errors */
        if (lsr & (LSR_OVERRUN_ERR | LSR_PARITY_ERR | LSR_FRAMING_ERR)) {
            /* drop or log — byte is still worth passing up in most designs */
        }
        text_console_putchar(&app.log, byte);
        com1_putchar(byte);
        if (byte == '\r') {
            com1_putchar('\n');
        }

    }


    outb(0x20, 0x20); // End Of Interrupt (EOI) to master PIC
    isr_end();
}


void app_init(struct app *app) {
    text_console_init(&app->log);
    text_console_putstr(&app->log, "app_init()\n");
    const struct idt_isr isr = {
        .keyboard_isr = keyboard_isr,
        .com1_isr     = com1_isr,
    };
    idt_init(&app->idt, &isr);
};





__attribute__ ((section (".kmain")))
void kmain(void) {

    // Before jumping here, the bootloader loads from media if needed,
    // enables A20, turns off interrupts, switches to protected mode
    // and jumps here.

    // Before doing anything, write something to the top right corner
    // of the screen.
    VIDEO[79*2] = '!';

    // initialize .bss segment
    mini_memset_volatile(&__bss_start, 0, &__bss_end - &__bss_start);

    // initialize app data
    app_init(&app);

    //volatile uint32_t *vw = (typeof(vw))0xB8000;

  loop:
    //(*vw)++;
    hlt();
    goto loop;
}
