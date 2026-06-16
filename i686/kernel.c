/* Kernel compiled with i686-elf-gcc.  Supports -march=i486 as well. */

/* Debug logging is defined globally before including any
   functionality.  If it is not defined, headers should assume LOG()
   to be a no-op.  It is too hard to try to always access this via
   object pointers. */

#if 1
int kernel_infof(const char *fmt, ...);
#define LOG(...) kernel_infof(__VA_ARGS__)
#else
#define LOG(...)
#endif

#define ASSERT_WARN(cond) { if (!(cond)) LOG("ASSERT FAILED: " #cond); }
#define ASSERT(cond) { if (!(cond)) ERROR("ASSERT FAILED: " #cond); }
#define ERROR(...) { LOG(__VA_ARGS__); halt(); }


#define for_device_phase1(m) \
    m(mcs9865) \
    m(mcs9904) \
    m(sunix) \

#define for_device_phase2(m) \
    m(rtl8139) \
    m(dp83815) \
    m(oxpcie952) \
    m(ox16pci952) \
    m(echo) \
    m(ice) \
    m(emu10k) \

#define for_device_disabled(m) \


#define for_device(m) \
    for_device_phase1(m) for_device_phase2(m)

#define STRUCT(name) struct name name;


/* Hardware access. */
#include "hw_i686_interrupts.h"
#include "hw_i686_com.h"
#include "hw_i686_spinner.h"
#include "hw_i686_rtl8139.h"
#include "hw_i686_mcs9865.h"
#include "hw_i686_mcs9904.h"
#include "hw_i686_dp83815.h"
#include "hw_i686_sunix.h"
#include "hw_i686_oxpcie952.h"
#include "hw_i686_ox16pci952.h"
#include "hw_i686_ice.h"
#include "hw_i686_echo.h"
#include "hw_i686_emu10k.h"

/* Firmware header */
#include "boot_config.h"

/* PC (VGA) text console. */
#include "text_console.h"

/* Telnet and ANSI terminal interface. */
#define TELNET_NO_INIT
#include "telnet.h"

/* Minimalistic UDP monitor to download kernel update. */
#include "mod_udp_mon.c"

/* All application state is in a single struct which make debugging a
   bit easier in case we ever do core dumps or gdb stub. */
struct app;
struct app {
    volatile uint32_t event;
    struct udp_mon udp_mon;
    struct uart com1;
    struct text_console log;
    struct telnet telnet;
    struct idt idt;
    void (*com_input)(struct app *, uint8_t);
    void (*com_output)(struct app *, uint8_t);
    uint32_t nb_zeros;
    for_device(STRUCT)
};
struct app g_app;





/* Comand and keyboard I/O
   - COM1 and PC keyboard can both be used as input
   - Text output goes to both COM1 and vga console
   - Text input is parsed by Telnet / ANSI terminal layer
   - And passed on to forth command interpreter */
static inline void app_com_putchar(struct app *app, uint8_t c) {

    // FIXME: It's better to just have one com port and set it with a
    // pointer.  Default would be com1.

    if (app->com1.irq) {
        uart_putchar(&app->com1, c);
    }
    if (app->mcs9865.uart[MCS9865_MAIN_UART].uart.irq) {
        uart_putchar(&app->mcs9865.uart[MCS9865_MAIN_UART].uart, c);
    }
    if (app->ox16pci952.uart.irq) {
        uart_putchar(&app->ox16pci952.uart, c);
    }
    if (app->sunix.uart.irq) {
        uart_putchar(&app->sunix.uart, c);
    }
}
static inline void app_info_putchar(struct app *app, uint8_t c) {
    text_console_putchar(&app->log, c);
    if (app->com_output) {
        app->com_output(app, c);
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
int kernel_infof(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    int rv = app_info_vf(&g_app, fmt, ap);
    va_end(ap);
    return rv;
}
/* COM port, Keyboard input */
void app_keyboard_input(struct app *app, uint8_t byte);
void app_monitor_input(struct app *app, uint8_t byte) {
    if (byte == 0) { app->nb_zeros++; }
    else { app->nb_zeros = 0; }
    if (app->nb_zeros >= 3) {
        app->com_input = app_keyboard_input;
        app->com_output = app_com_putchar;
        LOG("switch to commands\n");
    }
    monitor_3if_push_key(&app->udp_mon.monitor_3if, byte);
}
void app_keyboard_input(struct app *app, uint8_t byte) {
    if (byte == 0) {
        /* NULL is used to swtich from text command input to monitor
           input.  The monitor will switch back text command mode
           explicitly.  To test: terminal C-space sends NULL. */
        app->com_input = app_monitor_input;
        app->com_output = NULL;
        // FIXME: Make sure that text log is not going to com
        // This will only go to the text console.
        LOG("switched to monitor\n");
        app->nb_zeros = 0;
    }
    telnet_write_input(&app->telnet, &byte, 1);
}
void void_app_keyboard_input(void *vapp, uint8_t byte) {
    struct app *app = vapp;
    app_keyboard_input(app, byte);
}

/* COM port input can be re-routed to monitor. */
void app_com_input(void *vapp, uint8_t byte) {
    struct app *app = vapp;
    app->com_input(app, byte);
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
    (void)byte;
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
    uart_isr(&g_app.com1, (uart_sink_fn)app_com_input, &g_app);
    outb(0x20, 0x20); // End Of Interrupt (EOI) to master PIC
    isr_end();
}
__attribute__((naked))
static void rtl8139_isr(void) {
    isr_begin();
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
    uart_isr(&g_app.mcs9865.uart[MCS9865_MAIN_UART].uart, (uart_sink_fn)app_com_input, &g_app);
    outb(0xA0, 0x20); // End Of Interrupt (EOI) to slave PIC
    outb(0x20, 0x20); // End Of Interrupt (EOI) to master PIC
    isr_end();
}
__attribute__((naked))
static void ox16pci952_isr(void) {
    // This is only the first COM port
    isr_begin();
    static uint32_t count = 0;
    spinner(5, count++);
    uart_isr(&g_app.ox16pci952.uart, (uart_sink_fn)app_com_input, &g_app);
    outb(0xA0, 0x20); // End Of Interrupt (EOI) to slave PIC
    outb(0x20, 0x20); // End Of Interrupt (EOI) to master PIC
    isr_end();

}
__attribute__((naked))
static void sunix_isr(void) {
    isr_begin();
    static uint32_t count = 0;
    spinner(6, count++);
    uart_isr(&g_app.sunix.uart, (uart_sink_fn)app_com_input, &g_app);
    outb(0xA0, 0x20); // End Of Interrupt (EOI) to slave PIC
    outb(0x20, 0x20); // End Of Interrupt (EOI) to master PIC
    isr_end();
}

#define PCI_INIT(x) \
    if ((f->vendor == x##_vendor) && \
        (f->device == x##_device)) \
        x##_init(&app->x, f);


/* PCI scanning callback for driver instantiation. */
void pci_phase1(void *vapp, struct pci_function *f) {
    struct app *app = vapp;
    for_device_phase1(PCI_INIT)
}
void pci_phase2(void *vapp, struct pci_function *f) {
    struct app *app = vapp;
    // Imitate linux lspci -n
#if 1
    LOG("%02x:%02x.%d %02x%02x: %04x:%04x\n",
        f->bus, f->dev, f->func,
        f->class, f->subclass,
        f->vendor, f->device
        );
#endif
    for_device_phase2(PCI_INIT)
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
    monitor_3if_push_key(&app->udp_mon.monitor_3if, byte);
    // FIXME: This is the protocol switch conditiion.
    return 0;
}


void void_app_send(void *vapp, const uint8_t *data, uint32_t len) {
    // LOG("app_send %d\n", len);
    // log_hex(data, len);
    struct app *app = vapp;
    if (app->rtl8139.irq) {
        rtl8139_transmit(&app->rtl8139, data, len);
    }
}
void app_rx(void *vapp, const uint8_t *data, uint32_t len) {
    //LOG("app_rx %p\n", vapp);
    struct app *app = vapp;
    udp_mon_rx(&app->udp_mon, data, len);
}


/* Application init, called after memory is initialized. */
void app_init(struct app *app) {

    /* Start up with com port connected as command keyboard input. */
    app->com_input = app_keyboard_input;
    app->com_output = app_com_putchar;

    /* Init text console first, because LOG() depends on this.  Note
       that LOG() also depends on COM1 write but that seems to be ok
       without init for now: just use whatever BIOS configured in the
       UART. */
    text_console_init(&app->log);


#if 1
    g_app.com1.iobase = 0x3F8;
    g_app.com1.irq = 4;
    uart_init(&g_app.com1, 1);
    //uart_putstr(&g_app.com1, "com1 initialized\n");
#endif



    //text_console_putstr(&app->log, "app_init()\n");
    LOG("app_init %p\n", app);

    // Scan PCI bus before setting up interrupts.  Do this in two
    // phases to first get the serial ports initialized to log the
    // init phase of other devices.
    {
        struct pci_cb cb = { .fun = pci_phase1, .ctx = app };
        pci_enumerate(&cb);
    }
    {
        struct pci_cb cb = { .fun = pci_phase2, .ctx = app };
        pci_enumerate(&cb);
    }

    // initialize interrupt table
    // nonzero irq acts as enable for these
    struct idt_isr_entry isrs[] = {
        { .isr = keyboard_isr,   .irq = 1 },
        { .isr = com1_isr,       .irq = app->com1.irq },
        { .isr = rtl8139_isr,    .irq = app->rtl8139.irq },
        { .isr = dp83815_isr,    .irq = app->dp83815.irq },
        { .isr = mcs9865_isr,    .irq = app->mcs9865.uart[MCS9865_MAIN_UART].uart.irq },
        { .isr = ox16pci952_isr, .irq = app->ox16pci952.uart.irq },
        { .isr = sunix_isr,      .irq = app->sunix.uart.irq },
        {} // END-OF-LIST
    };

    idt_init(&app->idt, isrs);

    telnet_init(&app->telnet,
                telnet_write_output,
                telnet_event);

    // hook udp_mon state machine to network card if initialized
    if (app->rtl8139.irq) {
        LOG("connecting udp_mon to rtl8139\n");
        udp_mon_init(&app->udp_mon,
                     void_app_send,
                     void_app_keyboard_input,
                     app);
        app->udp_mon.mac = app->rtl8139.addr;
        app->rtl8139.ctx = app;
        app->rtl8139.rx  = app_rx; // last
        app->udp_mon.ip = boot_config.ip;
    }

    app->log.use_cli = 0;
    sti();

};

#include "ethernet.h"
void test_send(void) {

    const struct mac_addr mac_bc = {{0xFF,0xFF,0xFF,0xFF,0xFF,0xFF}};

#if 1
    struct __attribute__((packed)) {
        struct mac mac;
        uint32_t data;
    } packet;
    packet.mac.dst_mac = mac_bc;
    packet.mac.src_mac = g_app.rtl8139.addr;

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

void f1(void) {
    //par_pulse(&g_app.mcs9865.par);
    test_send();
}


void f2(void) {
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
    W(shutdown),   \
    W(f1),         \
    W(f2),         \

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
#if 0
void *memmove(void *dest, const void *src, size_t n) {
    return mini_memmove(dest, src, n);
}
#endif
int strcmp(const char *s1, const char *s2) {
    return mini_strcmp(s1, s2);
}
int memcmp(const void *s1, const void *s2, size_t len) {
    return mini_memcmp(s1, s2, len);
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
   enables A20, turns off interrupts, switches to protected mode.  The
   boot_config struct is located 512 bytes into the disk or NBP image
   and is loaded at 0x7E00, right after the boot sector at 0x7C00.
   Stack is set up below 7C00.*/


extern uint8_t __bss_start;
extern uint8_t __bss_end;

void entry(void) {

    /* Before doing anything, write something to the top right corner
       of the screen to indicate that we got at least this far.  The
       floppy loader writes a ? there before jumping here.  This will
       later be overwritten by the top status line from the
       text_console object. */
    VIDEO[79*2] = '!';

    /* Initialize memory.  */
    mini_memset_volatile(&__bss_start, 0, &__bss_end - &__bss_start);

    /* Initialize hardware and app functionality. */
    app_init(&g_app);

    LOG("version %s\n", VERSION);
    if (1) {
        LOG("%d size\n",  boot_config.endx - boot_config.start);
        LOG("%p start\n", boot_config.start);
        LOG("%p entry\n", boot_config.entry);
        LOG("%p app\n", boot_config.app);
        LOG("%p top\n", boot_config.top);
    }

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


__attribute__ ((section (".config")))
struct boot_config boot_config = {
    .entry = entry,
    .app   = &g_app,
    .top   = &__bss_end,
    .ip    = {{10,1,3,222}},
    .endx  = (void*)&__bss_start,
    .start = (void*)&boot_config,
};


