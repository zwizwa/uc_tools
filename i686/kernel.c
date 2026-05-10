/* Kernel compiled with i686-elf-gcc.  Supports -march=i486 as well. */

/* Debug logging is defined globally before including any
   functionality.  If it is not defined, headers should assume LOG()
   to be a no-op.  It is too hard to try to always access this via
   object pointers. */

#if 1
void kernel_infof(const char *fmt, ...);
#define LOG(...) kernel_infof(__VA_ARGS__)
#else
#define LOG(...)
#endif

#define for_device(m) \
    m(rtl8139) \
    m(dp83815) \
    m(mcs9865) \
    m(sunix) \

#define STRUCT(name) struct name name;


/* Hardware access. */
#include "hw_i686_interrupts.h"
#include "hw_i686_com.h"
#include "hw_i686_spinner.h"
#include "hw_i686_rtl8139.h"
#include "hw_i686_mcs9865.h"
#include "hw_i686_dp83815.h"
#include "hw_i686_sunix.h"

/* PC (VGA) text console. */
#include "text_console.h"

/* Telnet and ANSI terminal interface. */
#define TELNET_NO_INIT
#include "telnet.h"

/* Monitor vs. text command multiplexer. */
#include "mux_monitor.h"

/* Support for 3if monitor. */
#include "mod_monitor_3if.c"


/* All application state is in a single struct which make debugging a
   bit easier in case we ever do core dumps or gdb stub. */
struct app {
    volatile uint32_t event;
    struct uart com1;
    struct text_console log;
    struct telnet telnet;
    struct idt idt;
    struct mux_monitor mux_monitor;
    struct monitor_3if monitor_3if;
    for_device(STRUCT)
};
struct app g_app;





/* Comand and keyboard I/O
   - COM1 and PC keyboard can both be used as input
   - Text output goes to both COM1 and vga console
   - Text input is parsed by Telnet / ANSI terminal layer
   - And passed on to forth command interpreter */
static inline void app_info_putchar(struct app *app, char c) {
    text_console_putchar(&app->log, c);
    if (app->com1.irq) {
        uart_putchar(&app->com1, c);
    }
    if (app->mcs9865.uart.irq) {
        uart_putchar(&app->mcs9865.uart, c);
    }
    if (app->sunix.uart.irq) {
        uart_putchar(&app->sunix.uart, c);
    }
}
#define NS(tag) app_info_##tag
#define app_info_CTX_DEF struct app *app,
#define app_info_CTX_REF app,
#include "ns_infof.c"
#undef NS
#if 0
/* This is the "proper" way to do logging, but in practice it is too
   hard to not treat logging as globally accessible functionality, so
   just use the LOG() macro instead. */
static inline int app_infof(struct app *app, const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    int rv = app_info_vf(app, fmt, ap);
    va_end(ap);
    return rv;
}
#endif
/* Support LOG() without app reference. */
void kernel_infof(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    int rv = app_info_vf(&g_app, fmt, ap);
    va_end(ap);
}
/* COM port, Keyboard input */
void app_keyboard_input(void *vapp, uint8_t byte) {
    struct app *app = vapp;
    telnet_write_input(&app->telnet, &byte, 1);
}
void keyboard_input(uint8_t ascii) {
    app_keyboard_input(&g_app, ascii);
}
#include "mod_pc_keyboard.c"
/* Telnet output and input handler */
void forth_write(const uint8_t *line, uint32_t nb_char);
void telnet_write_output(struct telnet *, const uint8_t *bytes, uintptr_t len) {
    for (uintptr_t i=0; i<len; i++) {
        app_info_putchar(&g_app, bytes[i]);
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
        else {
            // forth_accept() expects white space termination
            // maybe move this into telnet.h code
            if (t->nb_char >= sizeof(t->line)) {
                t->nb_char = sizeof(t->line) - 1;
            }
            t->line[t->nb_char++] = '\n';
            forth_write(t->line, t->nb_char);
        }
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



/* ISRs */
__attribute__((naked))
static void keyboard_isr(void) {
    isr_begin();
    static uint32_t count = 0;
    spinner(0, count++);
    while (inb(0x64) & 1) {
        uint8_t scancode = inb(0x60);
        pc_keyboard_scancode(scancode);
    }
    outb(0x20, 0x20); // End Of Interrupt (EOI) to master PIC
    isr_end();
}
__attribute__((naked))
static void com1_isr(void) {
    isr_begin();
    static uint32_t count = 0;
    spinner(1, count++);
    uart_isr(&g_app.com1, (uart_sink_fn)app_keyboard_input, &g_app);
    outb(0x20, 0x20); // End Of Interrupt (EOI) to master PIC
    isr_end();
}
__attribute__((naked))
static void rtl8139_isr(void) {
    isr_begin();
    static uint32_t count = 0;
    rtl8139_isr_inner(&g_app.rtl8139);
    outb(0xA0, 0x20); // End Of Interrupt (EOI) to slave PIC
    outb(0x20, 0x20); // End Of Interrupt (EOI) to master PIC
    isr_end();
}
__attribute__((naked))
static void dp83815_isr(void) {
    isr_begin();
    static uint32_t count = 0;
    spinner(8, count++);
    dp83815_isr_inner(&g_app.dp83815);
    outb(0xA0, 0x20); // End Of Interrupt (EOI) to slave PIC
    outb(0x20, 0x20); // End Of Interrupt (EOI) to master PIC
    isr_end();
}
__attribute__((naked))
static void mcs9865_isr(void) {
    // This is only the first COM port
    isr_begin();
    static uint32_t count = 0;
    spinner(5, count++);
    uart_isr(&g_app.mcs9865.uart, (uart_sink_fn)app_keyboard_input, &g_app);
    outb(0xA0, 0x20); // End Of Interrupt (EOI) to slave PIC
    outb(0x20, 0x20); // End Of Interrupt (EOI) to master PIC
    isr_end();

}
__attribute__((naked))
static void sunix_isr(void) {
    isr_begin();
    static uint32_t count = 0;
    spinner(6, count++);
    uart_isr(&g_app.sunix.uart, (uart_sink_fn)app_keyboard_input, &g_app);
    outb(0xA0, 0x20); // End Of Interrupt (EOI) to slave PIC
    outb(0x20, 0x20); // End Of Interrupt (EOI) to master PIC
    isr_end();
}


/* PCI scanning callback for driver instantiation. */
void pci_cb_fn(void *vapp, struct pci_function *f) {
    struct app *app = vapp;
    // Imitate linux lspci -n
    LOG("%02x:%02x.%d %02x%02x: %04x:%04x\n",
        f->bus, f->dev, f->func,
        f->class, f->subclass,
        f->vendor, f->device
        );
    if ((f->vendor == RTL8139_VENDOR) &&
        (f->device == RTL8139_DEVICE)) {
        struct rtl8139 *d = &app->rtl8139;
        rtl8139_init(d, f);
    }
    if ((f->vendor == DP83815_VENDOR) &&
        (f->device == DP83815_DEVICE)) {
        struct dp83815 *d = &app->dp83815;
        dp83815_init(d, f);
    }
    if ((f->vendor == MCS9865_VENDOR) &&
        (f->device == MCS9865_DEVICE)) {
        struct mcs9865 *d = &app->mcs9865;
        mcs9865_init(d, f);
    }
    if ((f->vendor == SUNIX_VENDOR) &&
        (f->device == SUNIX_DEVICE)) {
        struct sunix *d = &app->sunix;
        sunix_init(d, f);
    }
}

void log_mem(uint32_t addr, uint32_t len) {
    LOG("%08x:\n", addr);
    log_hex((uint8_t*)addr, len);
}

void app_switch_to_monitor(void *vapp) {
    /* This means that logging should not go to the monitor port.  API
       probably needs to change to also witch back. */
    struct app *app = vapp;
    (void)app;
}
int app_mon_putchar(void *vapp, uint8_t byte) {
    /* This means that logging should not go to the monitor port.  API
       probably needs to change to also witch back. */
    struct app *app = vapp;
    monitor_3if_push_key(&app->monitor_3if, byte);\
    // FIXME: This is the protocol switch conditiion.
    return 0;
}


/* Application init, called after memory is initialized. */
void app_init(struct app *app) {

    /* Init text console first, because LOG() depends on this.  Note
       that LOG() also depends on COM1 write but that seems to be ok
       without init for now: just use whatever BIOS configured in the
       UART. */
    text_console_init(&app->log);

    /* The 3if monitor multiplexing s used in the logging path as well
       so set it up asap. */
    app->mux_monitor.app = &app;
    app->mux_monitor.app_putchar = app_keyboard_input;
    app->mux_monitor.app_switch_to_monitor = app_switch_to_monitor;
    app->mux_monitor.mon_putchar = app_mon_putchar;


#if 1
    g_app.com1.iobase = 0x3F8;
    g_app.com1.irq = 4;
    uart_init(&g_app.com1, 1);
    //uart_putstr(&g_app.com1, "com1 initialized\n");
#endif

    //text_console_putstr(&app->log, "app_init()\n");
    LOG("app_init %p\n", app);

    // scan PCI bus before setting up interrupts
    struct pci_cb cb = { .fun = pci_cb_fn, .ctx = app };
    pci_enumerate(&cb);

    // initialize interrupt table
    // nonzero irq acts as enable for these

    // FIXME: Turn this into a linked list, or an array that can be
    // defined in the "config space", e.g. kernel.c
    struct idt_isr isr = {
        .keyboard_isr = keyboard_isr,
        .com1         = { .isr = com1_isr,    .irq = app->com1.irq },
        .rtl8139      = { .isr = rtl8139_isr, .irq = app->rtl8139.irq },
        .dp83815      = { .isr = dp83815_isr, .irq = app->dp83815.irq },
        .mcs9865      = { .isr = mcs9865_isr, .irq = app->mcs9865.uart.irq },
        .sunix        = { .isr = sunix_isr,   .irq = app->sunix.uart.irq },
    };

    idt_init(&app->idt, &isr);

    telnet_init(&app->telnet,
                telnet_write_output,
                telnet_event);

    app->log.use_cli = 0;
    sti();

};

#include "ethernet.h"
void f1(void) {
#if 1
    struct __attribute__((packed)) {
        struct mac mac;
        uint32_t data;
    } packet;
    memset(packet.mac.d_mac, 0xFF, 6);
    memcpy(packet.mac.s_mac, g_app.rtl8139.mac, 6);
    packet.mac.ethertype = HTONS(0x88A4);
    packet.data          = HTONL(0x12345678);
#else
    uint8_t packet[] = {
        0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
        0x01,0x02,0x03,0x04,0x05,0x06,
        0x88,0xA4,
    };
#endif
    rtl8139_transmit(&g_app.rtl8139, &packet, sizeof(packet));
}



#if 1

/* The uc_tools Forth Instantiated at the end so it can easly
   reference all code in kernel.c compilation unit. */
#define FORTH_OUT_INFO 1
#include "tools.c"
#include "forth.h"
void hello(void) {
    LOG("hello!\n");
}
#define EVENT_RESTART (1<<0)
void restart(void) {
#if 0
    g_app.event |= EVENT_RESTART;
#else
    cli_and_restart();
#endif
}

#define W(word) {#word, (w)word}
#define FORTH_WORDS\
    W(hello),      \
    W(reboot),     \
    W(restart),    \
    W(f1),         \

#include "mod_forth.c"

#else
void forth_start(void) {}
void forth_write(const uint8_t *buf, uint32_t len) {}
#endif

/* Smaller re-implementations of libc functions.  Include these in the
   main image to override libc. */

void *memcpy(void *dest, const void *src, size_t n) {
    return mini_memcpy(dest, src, n);
}
int strcmp(const char *s1, const char *s2) {
    return mini_strcmp(s1, s2);
}
size_t strlen(const char *s1) {
    return mini_strlen(s1);
}
char *strcpy(char *dst, const char *src) {
    return mini_strcpy(dst, src);
}
void *memset(void *s, int c, size_t n) {
    return mini_memset(s, c, n);
}




/* Before jumping here, the bootloader loads from media if needed,
   enables A20, turns off interrupts, switches to protected mode.
   This code is located 512 bytes into the disk or NBP image and is
   loaded at 0x7E00, right after the boot sector at 0x7C00.  Stack is
   set up (below 7C00).*/
__attribute__ ((section (".kmain")))
__attribute__ ((naked))
void kmain(void) {

    /* Before doing anything, write something to the top right corner
       of the screen to indicate that we got at least this far.  The
       floppy loader writes a ? there before jumping here.  This will
       later be overwritten by the top status line from the
       text_console object. */
    VIDEO[79*2] = '!';

    /* Initialize memory.  */
    extern uint8_t __bss_start;
    extern uint8_t __bss_end;
    mini_memset_volatile(&__bss_start, 0, &__bss_end - &__bss_start);

    /* Initialize hardware and app functionality. */
    app_init(&g_app);

    LOG("version %s\n", VERSION);

    /* Start the forth interpreter. */
    forth_start();

    /* Main loop currently doesn't do anything.  Later this would be
       the place to handle non-real-time events, i.e. the "bottom
       half" interrupt routines. */
  loop:
    /* Wait for next interrupt.  This is crucial to make QEMU more
       efficient. */
    hlt();
    goto loop;
}


