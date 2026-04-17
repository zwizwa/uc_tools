#include <stdint.h>
#include "text_console.h"

extern uint8_t __bss_start;
extern uint8_t __bss_end;


// Put everything in a single static struct.
struct app {
    struct text_console log;
    struct idt idt;
};
void app_init(struct app *app) {
    text_console_init(&app->log);
    text_console_putstr(&app->log, "app_init()");
    // Initialize the interrupt controller.  Interrupts are on after
    // this, but all IRQs are masked.
    init_pic(&app->idt);
};

struct app app;

#if 0
void keyboard_handler(void) {
    uint8_t scancode = inb(0x60);
    // app.log.video[0] = scancode;
    (void)scancode;
    outb(0x20, 0x20); // End Of Interrupt (EOI) to master PIC
}
#endif

__attribute__((naked))
void keyboard_isr(void) {
    __asm__ volatile(
        "pusha"          "\n\t"
        "cld"            "\n\t"   // set movs direction
        "in $0x60, %al"  "\n\t"   // read scancode
        // "call keyboard_handler" "\n\t"
        "mov $0x20, %al" "\n\t"
        "out %al, $0x20" "\n\t"   // end-of-interrupt to master pic
        "popa"           "\n\t"
        "iret"           "\n\t"
    );
}



__attribute__ ((section (".kmain")))
void kmain(void) {

    // Before jumping here, the bootloader loads from media if needed,
    // enables A20, turns off interrupts, switches to protected mode
    // and jumps here.

    // initialize .bss segment to zero
    mini_memset(&__bss_start, 0, &__bss_end - &__bss_start);

    // initialize app data
    app_init(&app);

    // Enable keyboard interrupt
    outb(0x21, 0b11111101); // only IRQ1 unmasked

    volatile uint32_t *vw = (typeof(vw))0xB8000;

  loop:
    (*vw)++;
    hlt();
    goto loop;
}
