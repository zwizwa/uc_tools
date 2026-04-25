#include <stdint.h>


#if 1
void debugf(const char *fmt, ...);
#define LOG(...) debugf(__VA_ARGS__)
#else
#define LOG(...)
#endif

#include "text_console.h"

extern uint8_t __bss_start;
extern uint8_t __bss_end;


// Put everything in a single static struct.
struct app {
    struct text_console log;
    struct idt idt;
    struct rtl8139 rtl8139;
};

struct app g_app;

// Use this for debugging only if there is no direct path to app->log.
void debugf(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    int rv = text_console_info_vf(&g_app.log, fmt, ap);
    va_end(ap);
}


#define KBD_SHIFT  0x80
#define KBD_CTRL   0x81
#define KBD_ALT    0x82
#define KBD_CAPS   0x83

#define KBD_F1     0x90

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
    KBD_F1,  /* 59 - F1 key ... > */
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

void rtl8139_status(void) {
    struct rtl8139 *s = &g_app.rtl8139;
    text_console_infof(
        &g_app.log,
        "CBR=%04x CAPR=%04x MPC=%d ISR=%d CMD=%02x MSR=%02x RCR=%08x\n"
        ,inw(s->iobase + RTL_CBR)
        ,inw(s->iobase + RTL_CAPR)
        ,inl(s->iobase + RTL_MPC)
        ,inw(s->iobase + RTL_ISR)
        ,inb(s->iobase + RTL_CMD)
        ,inb(s->iobase + RTL_MSR)
        ,inl(s->iobase + RTL_RCR)
        );
}


__attribute__((naked))
static void keyboard_isr(void) {
    isr_begin();
    uint8_t scancode = inb(0x60);
    uint8_t ascii = kbd_US[scancode & 0x7F];
    // app.log.video[0] = ascii;
    if (!(scancode & 0x80)) {
        // press
        // FIXME: locking / buffering?
        if (ascii == KBD_F1) {
            rtl8139_status();
        }
        else {
            text_console_putchar(&g_app.log, ascii);
        }
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
        text_console_putchar(&g_app.log, byte);
        com1_putchar(byte);
        if (byte == '\r') {
            com1_putchar('\n');
        }

    }


    outb(0x20, 0x20); // End Of Interrupt (EOI) to master PIC
    isr_end();
}


__attribute__((naked))
static void rtl8139_isr(void) {
    isr_begin();
    //LOG("rt8139 isr\n");
    rtl8139_isr_inner(&g_app.rtl8139);
    outb(0xA0, 0x20); // End Of Interrupt (EOI) to slave PIC
    outb(0x20, 0x20); // End Of Interrupt (EOI) to master PIC
    isr_end();
}


void pci_cb_fn(void *vapp, struct pci_function *f) {
    struct app *app = vapp;
    text_console_infof(
        &app->log,
        // Imitate linux lspci -n
        "%02x:%02x.%d %02x%02x: %04x:%04x\n",
        f->bus, f->dev, f->func,
        f->class, f->subclass,
        f->vendor, f->device
        );
    if ((f->vendor == 0x10ec) &&
        (f->device == 0x8139)) {
        struct rtl8139 *rtl = &app->rtl8139;
        rtl8139_init(rtl, f);
        text_console_infof(
            &app->log,
            "rtl8139 io=%04x irq=%d mac=%02x:%02x:%02x:%02x:%02x:%02x\n",
            rtl->iobase,
            rtl->irq,
            rtl->mac[0],
            rtl->mac[1],
            rtl->mac[2],
            rtl->mac[3],
            rtl->mac[4],
            rtl->mac[5]);
    }
}

void app_init(struct app *app) {
    text_console_init(&app->log);
    //text_console_putstr(&app->log, "app_init()\n");
    text_console_infof(&app->log, "app_init %p\n", app);

    // scan PCI bus before setting up interrupts
    struct pci_cb cb = { .fun = pci_cb_fn, .ctx = app };
    pci_enumerate(&cb);

    // initialize interrupt table
    struct idt_isr isr = {
        .keyboard_isr = keyboard_isr,
        .com1_isr     = com1_isr,
        .rtl8139      = { .isr = rtl8139_isr },
    };
    isr.rtl8139.irq = app->rtl8139.irq; // nonzero acts as enable

    idt_init(&app->idt, &isr);

    app->log.use_cli = 0;
    sti();

};





__attribute__ ((section (".kmain")))
void kmain(void) {

    // Before jumping here, the bootloader loads from media if needed,
    // enables A20, turns off interrupts, switches to protected mode
    // and jumps here.

    // Before doing anything, write something to the top right corner
    // of the screen.  This is for the floppy loader which writes a ?
    // there.
    VIDEO[79*2] = '!';

    // initialize .bss segment
    mini_memset_volatile(
        &__bss_start,
        0,
        &__bss_end - &__bss_start);

    // initialize app data
    app_init(&g_app);

    //volatile uint32_t *vw = (typeof(vw))0xB8000;

  loop:
    //(*vw)++;
    hlt();
    goto loop;
}
