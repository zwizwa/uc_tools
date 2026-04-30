#include <stdint.h>
#include <string.h>


#if 1
void debugf(const char *fmt, ...);
#define LOG(...) debugf(__VA_ARGS__)
#else
#define LOG(...)
#endif

/* PC (VGA) text console. */
#include "text_console.h"

/* Command line editor. */
#include "line_editor.h"

/* Telnet and ANSI terminal interface. */
#define TELNET_NO_INIT
#include "telnet.h"


extern uint8_t __bss_start;
extern uint8_t __bss_end;


// Put everything in a single static struct.
struct app {
    struct text_console log;
    struct idt idt;
    struct rtl8139 rtl8139;
    struct telnet telnet;
    struct line_editor line_editor;
    struct pbuf line_editor_pbuf;
    uint8_t line_editor_pbuf_buf[128];
};

struct app g_app;



/* Instantiate printf-style logging on top of _putchar for both text
   console ans serial port. */
static inline void debug_putchar(struct app *app, char c) {
    text_console_putchar(&app->log, c);
    com1_putchar(c);
}
#define NS(tag) debug_##tag
#define debug_CTX_DEF struct app *app,
#define debug_CTX_REF app,
#include "ns_infof.c"
#undef NS
static inline int debug_infof(struct app *app, const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    int rv = debug_vf(app, fmt, ap);
    va_end(ap);
    return rv;
}
// Note that it is really difficult to make this refer to app because
// I want logging to be available everywhere.  So I guess it is ok.
void debugf(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    int rv = debug_vf(&g_app, fmt, ap);
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
    'q', 'w', 'e', 'r', 't', 'y', 'u', 'i', 'o', 'p', '[', ']', '\r',
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
    debug_infof(
        &g_app,
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

void forth_write(const uint8_t *buf, uint32_t len);

/* Send a raw character from serial port terminal to the command
   interpreter and perform echo with the needed CR LF conversion.

   This is how I understand the convention:

   - The ENTER key coming in on COM1 looks like '\r'
   - The '\r and '\n' characters sent to COM1 separate CR and LF
   - The '\r' in printf is CR, the '\n' is CR,LF

*/

void app_keyboard_input(struct app *app, uint8_t byte) {
#if 0
    debug_putchar(app, byte);
    if (byte == '\r') {
        debug_putchar(app, '\n');
    }
    forth_write(&byte, 1);
#elseif 0
    line_editor_push(&app->line_editor, byte);
#else
    telnet_write_input(&app->telnet, &byte, 1);
#endif
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
        else if (ascii == 27) {
            // Keyboard controller (8042) reset — pulse the CPU reset line
            // Some alternatives here:
            // https://claude.ai/chat/0be89304-9906-4b33-bf8c-f11e477fda0c
            reboot();
        }
        else {
            // The encoding emulates a serial terminal.
            app_keyboard_input(&g_app, ascii);
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

        if (1) {
            // FIXME: This seems to crash.
            static const uint8_t hex[16] = "0123456789ABCDEF";
            volatile uint8_t *v = (void*)0xB8000;
            v[0] = hex[(byte >> 4) & 0xF]; v[1] = 0x17;
            v[2] = hex[(byte >> 0) & 0xF]; v[3] = 0x17;
        }

        /* Optionally inspect lsr for framing/parity/overrun errors */
        if (lsr & (LSR_OVERRUN_ERR | LSR_PARITY_ERR | LSR_FRAMING_ERR)) {
            /* drop or log — byte is still worth passing up in most designs */
        }
        app_keyboard_input(&g_app, byte);
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
    debug_infof(
        app,
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
        debug_infof(
            app,
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


void app_echo(void *vapp, uint8_t byte) {
    struct app *app = vapp;
    text_console_putchar(&app->log, byte);
    com1_putchar(byte);
}
void app_line(void *vapp, const uint8_t *buf, uint32_t bytes) {
    // FIXME
}


void telnet_write_output(struct telnet *, const uint8_t *bytes, uintptr_t len) {
    for (uintptr_t i=0; i<len; i++) {
        debug_putchar(&g_app, bytes[i]);
    }
}
void telnet_event(struct telnet *t, uintptr_t event) {
    //LOG("event 0x%x\n", event);
    uint8_t byte = event & 0xFF;
    event &= ~0xff;
    switch(event) {
    case TELNET_EVENT_LINE:
        if (0) {
            LOG("<LINE:");
            for(uint32_t i=0; i<t->nb_char; i++) {
                LOG("%c", t->line[i]);
            }
            LOG(">\n");
        }
        // forth_accept() expects white space termination
        // maybe move this into telnet.h code
        if (t->nb_char >= sizeof(t->line)) {
            t->nb_char = sizeof(t->line) - 1; 
        }
        t->line[t->nb_char++] = '\n';
        forth_write(t->line, t->nb_char);
        break;
    case TELNET_EVENT_ESCAPE:
        LOG("<ESC:");
        for(uint32_t i=0; i<t->nb_esc; i++) {
            LOG("%c", t->esc[i]);
        }
        LOG(">\n");
        break;
    default:
        break;
    }
}

void app_init(struct app *app) {

    PBUF_INIT(app->line_editor_pbuf);
    line_editor_init(&app->line_editor,
                     &app->line_editor_pbuf,
                     app_echo,
                     app_line,
                     app);

    text_console_init(&app->log);
    //text_console_putstr(&app->log, "app_init()\n");
    debug_infof(app, "app_init %p\n", app);

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

    telnet_init(&app->telnet,
                telnet_write_output,
                telnet_event);

    app->log.use_cli = 0;
    sti();

};



/* The uc_tools Forth */
#define strlen mini_strlen
#define strcmp mini_strcmp
#define FORTH_OUT_INFO 1
#include "tools.c"
#include "forth.h"
void hello(void) {
    LOG("hello!\n");
}
void reboot(void) {
    LOG("reboot...\n");
    outb(0x64, 0xFE);
}
#define FORTH_WORDS        \
    {"hello",  (w)hello},  \
    {"reboot", (w)reboot}, \

#include "mod_forth.c"




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
    // forth_write_word("hello");

    forth_start();

  loop:
    //(*vw)++;
    hlt();
    goto loop;
}



