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


__attribute__((naked))
static void keyboard_isr(void) {
    isr_begin();
    uint8_t scancode = inb(0x60);
    app.log.video[0] = scancode;
    outb(0x20, 0x20); // End Of Interrupt (EOI) to master PIC
    isr_end();
}


void app_init(struct app *app) {
    text_console_init(&app->log);
    text_console_putstr(&app->log, "app_init()");
    idt_init(&app->idt,
             keyboard_isr);
};





__attribute__ ((section (".kmain")))
void kmain(void) {

    // Before jumping here, the bootloader loads from media if needed,
    // enables A20, turns off interrupts, switches to protected mode
    // and jumps here.

    // initialize .bss segment to zero
    mini_memset(&__bss_start, 0, &__bss_end - &__bss_start);

    // initialize app data
    app_init(&app);


    //volatile uint32_t *vw = (typeof(vw))0xB8000;

  loop:
    //(*vw)++;
    hlt();
    goto loop;
}
