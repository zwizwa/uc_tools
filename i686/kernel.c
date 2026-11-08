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
    m(oxpcie952) \
    m(ox16pci952) \
    m(echo) \
    m(ice) \
    m(emu10k) \

/* Network cards, with the name of the mac field and more device ids. */
#define for_device_net(m) \
    m(rtl8139, addr, 0, 0, 0, 0, 0) \
    m(dp83815, mac, 0, 0, 0, 0, 0) \
    m(i82574, mac, i82579_device, i82566_device, i82577_device, i82567_device, \
      i82544_device) \
    m(rtl8168, mac, rtl8169_device, 0, 0, 0, 0) \

#define for_device_disabled(m) \


#define for_device(m) \
    for_device_phase1(m) for_device_phase2(m)

#define STRUCT(name) struct name name;
#define STRUCT_NET(name, mac, device2, device3, device4, device5, device6) \
    STRUCT(name)


/* Hardware access. */
#include "hw_i686_interrupts.h"
#include "hw_i686_com.h"
#include "hw_i686_pit.h"
#include "hw_i686_spinner.h"
#include "hw_i686_rtl8139.h"
#include "hw_i686_mcs9865.h"
#include "hw_i686_mcs9904.h"
#include "hw_i686_dp83815.h"
#include "hw_i686_i82574.h"
#include "hw_i686_rtl8168.h"
#include "hw_i686_sunix.h"
#include "hw_i686_oxpcie952.h"
#include "hw_i686_ox16pci952.h"
#include "hw_i686_ice.h"
#include "hw_i686_echo.h"
#include "hw_i686_emu10k.h"
#include "hw_i686_ahci.h"
#include "hw_i686_ata_pio.h"
#include "plugin.h"

/* Network card used by udp_mon: first one in PCI order */
struct net {
    const char *name;
    void *dev;  // zero: no card
    int (*transmit)(void *, const void *, uint16_t);
    struct mac_addr *mac;
    void (**rx)(void *, const uint8_t *, uint32_t);
    void **ctx;
};

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
/* Events from the isrs to the main loop, see plugin.h */
struct kq {
    volatile uint32_t write;
    volatile uint32_t read;
    uint32_t size;
    uint8_t *buf;
    uint32_t reserved;
};
typedef struct kq kq_queue_t;
#define NS(name) kq##name
#include "ns_rqueue.h"
#undef NS

struct app {
    volatile uint32_t event;
    struct kq kq;
    uint8_t kq_buf[0x10000] __attribute__((aligned(4)));  // no text loss
    uint32_t kq_drops;   // isr
    uint32_t kq_events;  // main loop
    uint32_t nb_lines;   // main loop
    volatile uint32_t main_loop;  // set when isrs can queue LOG text
    struct udp_mon udp_mon;
    struct uart com1;
    struct uart_tx com1_tx;  // console output, main loop -> isr
    struct text_console log;
    struct telnet telnet;
    struct idt idt;
    void (*com_input)(struct app *, uint8_t);
    void (*com_output)(struct app *, uint8_t);
    uint32_t nb_zeros;
    for_device(STRUCT)
    for_device_net(STRUCT_NET)
    struct ahci ahci;
    struct ata_pio ata;     // legacy IDE primary master, when no ahci disk
    struct partition part;  // type 7f on the disk, for sqlite
    struct net net;
};
struct app g_app;

/* See plugin.h */
struct blockdev *kernel_blockdev;
const char kernel_version[] = VERSION;





/* Comand and keyboard I/O
   - COM1 and PC keyboard can both be used as input
   - Text output goes to both COM1 and vga console
   - Text input is parsed by Telnet / ANSI terminal layer
   - And passed on to forth command interpreter */
/* COM1 is interrupt driven, no text loss: wait for room, or poll the
   isr when interrupts are off (boot). */
static inline void app_com1_put(struct app *app, uint8_t c) {
    while (uart_tx_put(&app->com1, &app->com1_tx, c)) {
        if (!interrupts_enabled()) {
            uart_tx_isr(&app->com1, &app->com1_tx);
            continue;
        }
        cli();
        if (uart_tx_full(&app->com1_tx)) __asm__ volatile ("sti; hlt");
        else sti();
    }
}
static inline void app_com1_putchar(struct app *app, uint8_t c) {
    if (c == '\n') app_com1_put(app, '\r');
    app_com1_put(app, c);
}
static inline void app_com_putchar(struct app *app, uint8_t c) {

    // FIXME: It's better to just have one com port and set it with a
    // pointer.  Default would be com1.

    if (app->com1.irq) {
        app_com1_putchar(app, c);
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
void (*kernel_console_hook)(uint8_t c);
static inline void app_info_putchar(struct app *app, uint8_t c) {
    text_console_putchar(&app->log, c);
    // boot_config.plugin is cleared before a plugin image is written
    if (kernel_console_hook && boot_config.plugin) kernel_console_hook(c);
    // log ring and tx are shared with the isrs
    int ie = interrupts_enabled();
    if (ie) cli();
    udp_mon_log_putchar(&app->udp_mon, c);
    if (ie) sti();
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
/* LOG from an isr is formatted here and printed by the main loop.
   isrs don't nest, so one buffer. */
struct isr_log {
    uint8_t buf[256];
    uint32_t len;
};
struct isr_log isr_log;
static inline void isr_info_putchar(struct isr_log *l, uint8_t c) {
    if (l->len < sizeof(l->buf)) l->buf[l->len++] = c;
}
#define NS(tag) isr_info_##tag
#define isr_info_CTX_DEF struct isr_log *l,
#define isr_info_CTX_REF l,
#include "ns_infof.c"
#undef NS

/* Support LOG() without app reference.  In an isr: queue the text as
   KERNEL_EVENT_LOG.  During boot interrupts are off too, but there is
   no main loop yet: print directly. */
int kernel_infof(const char *fmt, ...) {
    va_list ap;
    va_start(ap, fmt);
    int rv;
    if (interrupts_enabled() || !g_app.main_loop) {
        rv = app_info_vf(&g_app, fmt, ap);
    }
    else {
        struct isr_log *l = &isr_log;
        l->len = 0;
        rv = isr_info_vf(l, fmt, ap);
        uint8_t *d = kq_reserve(&g_app.kq, KERNEL_EVENT_LOG, l->len);
        if (d) {
            mini_memcpy(d, l->buf, l->len);
            kq_commit(&g_app.kq);
        }
        else {
            g_app.kq_drops++;
        }
    }
    va_end(ap);
    return rv;
}
/* Console input from the isrs, telnet runs in the main loop. */
static void app_input(struct app *app, const uint8_t *data, uint32_t len, int lf_to_cr) {
    uint8_t *d = kq_reserve(&app->kq, KERNEL_EVENT_INPUT, len);
    if (!d) {
        app->kq_drops++;
        return;
    }
    for (uint32_t i = 0; i < len; i++) {
        d[i] = (lf_to_cr && data[i] == '\n') ? '\r' : data[i];
    }
    kq_commit(&app->kq);
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
    app_input(app, &byte, 1, 0);
}
/* UDP console, one event per packet */
void void_app_console_input(void *vapp, const uint8_t *data, uint32_t len) {
    app_input(vapp, data, len, 1);
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
static void kernel_event(uint32_t tag, const uint8_t *data, uint32_t len);
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
            kernel_event(KERNEL_EVENT_LINE, t->line, t->nb_char);
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
/* Commands from the main loop to one isr: ns_rqueue, main loop is the
   only producer.  A failed post sets want_room, the isr answers with
   KERNEL_EVENT_ROOM after its next drain.  want_room is written by
   both sides, the worst case is a spurious ROOM event. */
struct isr_cmdq {
    struct kq q;
    volatile uint32_t want_room;
    uint32_t id;  // KERNEL_CMDQ_*
};
static int isr_cmdq_post(struct isr_cmdq *c, uint16_t tag, const void *data, uint32_t len) {
    uint8_t *d = kq_reserve(&c->q, tag, len);
    if (!d) {
        c->want_room = 1;
        return -1;
    }
    mini_memcpy(d, data, len);
    kq_commit(&c->q);
    return 0;
}
/* Isr, after draining. */
static void isr_cmdq_room(struct isr_cmdq *c) {
    if (!c->want_room) return;
    uint8_t *d = kq_reserve(&g_app.kq, KERNEL_EVENT_ROOM, 4);
    if (!d) return;  // next time
    mini_memcpy(d, &c->id, 4);
    kq_commit(&g_app.kq);
    c->want_room = 0;
}

/* PIT tick and alarms, see kernel_timer_at in plugin.h */
#define TIMER_HZ 1000
#define TIMER_NB_ALARM 16
#define TIMER_CMD_AT 0
struct timer_alarm {
    uint32_t at, cookie, used;
};
struct timer {
    struct isr_cmdq cmdq;
    uint8_t cmdq_buf[1024] __attribute__((aligned(4)));
    struct timer_alarm alarm[TIMER_NB_ALARM];
};
struct timer timer;
volatile uint32_t kernel_ms;
int kernel_timer_at(uint32_t at, uint32_t cookie) {
    uint32_t cmd[2] = { at, cookie };
    return isr_cmdq_post(&timer.cmdq, TIMER_CMD_AT, cmd, sizeof(cmd));
}
/* noinline: the naked isr has no frame, inlined code would use the
   interrupted code's %ebp for its locals. */
__attribute__((noinline))
static void timer_service(void) {
    kernel_ms++;
    /* Commands stay queued while the alarm table is full. */
    for (;;) {
        uint16_t tag, len;
        uint8_t *d = kq_peek(&timer.cmdq.q, &tag, &len);
        if (!d) break;
        struct timer_alarm *a = 0;
        for (int i = 0; i < TIMER_NB_ALARM; i++) {
            if (!timer.alarm[i].used) { a = &timer.alarm[i]; break; }
        }
        if (!a) break;
        mini_memcpy(a, d, 8);
        a->used = 1;
        kq_release(&timer.cmdq.q);
    }
    isr_cmdq_room(&timer.cmdq);
    /* A due alarm stays when the event queue is full. */
    for (int i = 0; i < TIMER_NB_ALARM; i++) {
        struct timer_alarm *a = &timer.alarm[i];
        if (!a->used || (int32_t)(kernel_ms - a->at) < 0) continue;
        uint8_t *d = kq_reserve(&g_app.kq, KERNEL_EVENT_TIMER,
                                sizeof(struct kernel_timer_event));
        if (!d) break;
        struct kernel_timer_event e = { .cookie = a->cookie, .ms = kernel_ms };
        mini_memcpy(d, &e, sizeof(e));
        kq_commit(&g_app.kq);
        a->used = 0;
    }
}
__attribute__((naked))
static void timer_isr(void) {
    isr_begin();
    timer_service();
    outb(0x20, 0x20); // End Of Interrupt (EOI) to master PIC
    isr_end();
}

/* Edge triggered: service until nothing is pending, rx and tx.
   noinline, see timer_service. */
__attribute__((noinline))
static void com1_service(void) {
    while (!(uart_in(&g_app.com1, UART_IIR) & IIR_NONE)) {
        uart_isr(&g_app.com1, (uart_sink_fn)app_com_input, &g_app);
        uart_tx_isr(&g_app.com1, &g_app.com1_tx);
    }
}
__attribute__((naked))
static void com1_isr(void) {
    isr_begin();
    static uint32_t count = 0;
    spinner(1, count++);
    com1_service();
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
static void i82574_isr(void) {
    isr_begin();
    static uint32_t count = 0;
    spinner(8, count++);
    i82574_isr_inner(&g_app.i82574);
    outb(0xA0, 0x20); // End Of Interrupt (EOI) to slave PIC
    outb(0x20, 0x20); // End Of Interrupt (EOI) to master PIC
    isr_end();
}
__attribute__((naked))
static void rtl8168_isr(void) {
    isr_begin();
    static uint32_t count = 0;
    spinner(8, count++);
    rtl8168_isr_inner(&g_app.rtl8168);
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
static void oxpcie952_isr(void) {
    isr_begin();
    static uint32_t count = 0;
    spinner(7, count++);
    oxpcie952_isr_inner(&g_app.oxpcie952);
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

/* Other cards stay untouched: no irq, so no isr. */
#define PCI_INIT_NET(x, mac_field, device2, device3, device4, device5, device6) \
    if (!app->net.dev && \
        (f->vendor == x##_vendor) && \
        ((f->device == x##_device) || (f->device == device2) || \
         (f->device == device3) || (f->device == device4) || \
         (f->device == device5) || (f->device == device6))) { \
        x##_init(&app->x, f); \
        app->net = (struct net){ \
            .name     = #x, \
            .dev      = &app->x, \
            .transmit = (void*)x##_transmit, \
            .mac      = &app->x.mac_field, \
            .rx       = &app->x.rx, \
            .ctx      = &app->x.ctx, \
        }; \
    }


/* PCI scanning callback for driver instantiation. */
void pci_phase1(void *vapp, struct pci_function *f) {
    struct app *app = vapp;
    for_device_phase1(PCI_INIT)
}
void pci_phase2(void *vapp, struct pci_function *f) {
    struct app *app = vapp;
    // Imitate linux lspci -n
#if 0
    LOG("%02x:%02x.%d %02x%02x: %04x:%04x\n",
        f->bus, f->dev, f->func,
        f->class, f->subclass,
        f->vendor, f->device
        );
#endif
    for_device_phase2(PCI_INIT)
    for_device_net(PCI_INIT_NET)
    if (f->class == 0x01 && f->subclass == 0x06 && !app->ahci.port)
        ahci_init(&app->ahci, f);
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
    if (app->net.dev) {
        app->net.transmit(app->net.dev, data, len);
    }
}
int app_log_send(void *vapp, const uint8_t *data, uint32_t len) {
    struct app *app = vapp;
    return app->net.transmit(app->net.dev, data, len);
}
/* Frames for KERNEL_EVENT_NET, see plugin.h */
static int net_for_plugin(const uint8_t *data, uint32_t len) {
    const struct eth_udp *p = (const void*)data;
    if (len < sizeof(struct mac)) return 0;
    uint16_t ethertype = NTOHS(p->mac.ethertype);
    if (ethertype == ETHERTYPE_ARP) return 1;
    if (ethertype != ETHERTYPE_IPV4 || len < sizeof(struct mac) + sizeof(struct ip)) return 0;
    if (p->ip.protocol == PROTOCOL_ICMP) return 0;
    if (p->ip.protocol != PROTOCOL_UDP || len < sizeof(*p)) return 1;
    uint16_t port = HTONS(p->udp.dst_port);
    return port != UDP_MON_CONSOLE_PORT && port != UDP_MON_COMMAND_PORT;
}
void app_rx(void *vapp, const uint8_t *data, uint32_t len) {
    //LOG("app_rx %p\n", vapp);
    struct app *app = vapp;
    udp_mon_rx(&app->udp_mon, data, len);
    struct plugin *p = boot_config.plugin;
    if (p && (p->events & (1 << KERNEL_EVENT_NET)) && net_for_plugin(data, len)) {
        uint8_t *d = kq_reserve(&app->kq, KERNEL_EVENT_NET, len);
        if (d) {
            mini_memcpy(d, data, len);
            kq_commit(&app->kq);
        }
        else {
            app->kq_drops++;
        }
    }
    /* Doorbell rung by 3if, see plugin.h.  Left set when the queue is
       full, so the next packet retries. */
    if (boot_config.plugin_load) {
        uint8_t *d = kq_reserve(&app->kq, KERNEL_EVENT_PLUGIN, 4);
        if (d) {
            mini_memcpy(d, &boot_config.plugin_load, 4);
            kq_commit(&app->kq);
            boot_config.plugin_load = 0;
        }
    }
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
    // Absent on the X230: tx would wait for a THRE irq forever.
    if (uart_present(&g_app.com1)) {
        g_app.com1.irq = 4;
        uart_init(&g_app.com1, 1);
    }
    LOG("com1 %s\n", g_app.com1.irq ? "16550" : "absent");
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
    struct blockdev *disk = 0;
    if (app->ahci.port) {
        disk = &app->ahci.bd;
    }
    else {
        // Soekris CF is the slave
        for (int slave = 0; slave < 2 && !disk; slave++) {
            if (!ata_pio_init(&app->ata, ATA_PRIMARY_IO, ATA_PRIMARY_CTL, slave)) disk = &app->ata.bd;
        }
    }
    if (disk) {
        if (partition_find(&app->part, disk, 0x7f)) {
            LOG("no type 7f partition\n");
        }
        else {
            LOG("partition 7f lba0=%d sectors=%d\n",
                app->part.lba0, app->part.bd.nb_sectors);
            // sqlite writes 4K blocks, see mod_sqlite_vfs.c
            if (app->part.lba0 % 8) LOG("partition 7f not 4K aligned\n");
            kernel_blockdev = &app->part.bd;
        }
    }

    // initialize interrupt table
    // nonzero irq acts as enable for these
    struct idt_isr_entry isrs[] = {
        { .isr = keyboard_isr,   .irq = 1 },
        { .isr = com1_isr,       .irq = app->com1.irq },
        { .isr = rtl8139_isr,    .irq = app->rtl8139.irq },
        { .isr = dp83815_isr,    .irq = app->dp83815.irq },
        { .isr = i82574_isr,     .irq = app->i82574.irq },
        { .isr = rtl8168_isr,    .irq = app->rtl8168.irq },
        { .isr = mcs9865_isr,    .irq = app->mcs9865.uart[MCS9865_MAIN_UART].uart.irq },
        { .isr = ox16pci952_isr, .irq = app->ox16pci952.uart.irq },
        { .isr = sunix_isr,      .irq = app->sunix.uart.irq },
        { .isr = oxpcie952_isr,  .irq = app->oxpcie952.irq },
        {} // END-OF-LIST
    };

    kq_init(&app->kq, app->kq_buf, sizeof(app->kq_buf));
    kq_init(&timer.cmdq.q, timer.cmdq_buf, sizeof(timer.cmdq_buf));
    timer.cmdq.id = KERNEL_CMDQ_TIMER;
    idt_init(&app->idt, isrs);
    /* irq 0 means "off" in isrs[], so the PIT goes in here. */
    idt_set(&app->idt, 0, timer_isr);
    outb(0x21, ~app->idt.master_enable);
    pit_init(TIMER_HZ);

    telnet_init(&app->telnet,
                telnet_write_output,
                telnet_event);

    // hook udp_mon state machine to network card if initialized
    if (app->net.dev) {
        LOG("connecting udp_mon to %s\n", app->net.name);
        udp_mon_init(&app->udp_mon,
                     void_app_send,
                     void_app_console_input,
                     app);
        app->udp_mon.log_send = app_log_send;
        app->udp_mon.mac = *app->net.mac;
        *app->net.ctx = app;
        *app->net.rx  = app_rx; // last
        app->udp_mon.ip = boot_config.ip;
        LOG("ip "); log_ipv4(&app->udp_mon.ip); LOG("\n");
    }

    app->log.use_cli = 0;
    // From here isr LOGs go to the queue, see kernel_infof
    app->main_loop = 1;
    sti();

};

#include "ethernet.h"
void test_send(void) {
    if (!g_app.net.dev) return;

    const struct mac_addr mac_bc = {{0xFF,0xFF,0xFF,0xFF,0xFF,0xFF}};

#if 1
    struct __attribute__((packed)) {
        struct mac mac;
        uint32_t data;
    } packet;
    packet.mac.dst_mac = mac_bc;
    packet.mac.src_mac = *g_app.net.mac;

    packet.mac.ethertype = HTONS(0x88A4);
    packet.data          = HTONL(0x12345678);
#else
    uint8_t packet[] = {
        0xFF,0xFF,0xFF,0xFF,0xFF,0xFF,
        0x01,0x02,0x03,0x04,0x05,0x06,
        0x88,0xA4,
    };
#endif
    // tx_head is shared with the rx isr
    int ie = interrupts_enabled();
    if (ie) cli();
    g_app.net.transmit(g_app.net.dev, &packet, sizeof(packet));
    if (ie) sti();
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
static void lspci_function(void *ctx, struct pci_function *f) {
    LOG("%02x:%02x.%d %02x%02x: %04x:%04x\n",
        f->bus, f->dev, f->func,
        f->class, f->subclass,
        f->vendor, f->device);
}
void lspci(void) {
    struct pci_cb cb = { .fun = lspci_function };
    pci_enumerate(&cb);
}

void netstat(void) {
    if (g_app.i82574.mmio) i82574_stats(&g_app.i82574);
    if (g_app.rtl8168.iobase) rtl8168_stats(&g_app.rtl8168);
    if (g_app.rtl8139.iobase) rtl8139_status(&g_app.rtl8139);
}

/* oxpcie952 uart 0 test */
void oxsend(void) {
    if (g_app.oxpcie952.irq) uart_putstr(&g_app.oxpcie952.uart[0], "hello\n");
}
void oxloop(void) {
    struct uart *u = &g_app.oxpcie952.uart[0];
    if (u->mmio) uart_out(u, UART_MCR, uart_in(u, UART_MCR) ^ 0x10);
}
void oxrx(void) {
    struct oxpcie952 *s = &g_app.oxpcie952;
    LOG("rx_head=%d\n", s->rx_head);
    log_hex(s->rx, s->rx_head);
    s->rx_head = 0;
}

/* Main loop, see plugin.h */
static void plugin_load(const struct plugin_header *h) {
    char v[sizeof(h->kernel_version) + 1] = {};
    if (h->magic != PLUGIN_MAGIC) {
        LOG("plugin bad magic %08x\n", h->magic);
        return;
    }
    mini_memcpy(v, h->kernel_version, sizeof(h->kernel_version));
    if (mini_strcmp(v, kernel_version)) {
        LOG("plugin built for kernel %s, this is %s\n", v, kernel_version);
        return;
    }
    kernel_console_hook = 0;
    mini_memset(h->bss_start, 0, h->bss_end - h->bss_start);
    struct plugin *p = h->init();
    boot_config.plugin = p;
    if (p) LOG("plugin %s\n", p->name);
}

/* See plugin.h */
int kernel_net_send(const void *frame, uint32_t len) {
    if (!g_app.net.dev) return -1;
    cli();  // the net isr transmits too
    int rv = g_app.net.transmit(g_app.net.dev, frame, len);
    sti();
    return rv;
}
void kernel_net_addr(uint8_t mac[6], uint8_t ip[4]) {
    mini_memcpy(mac, &g_app.udp_mon.mac, 6);
    mini_memcpy(ip, &g_app.udp_mon.ip, 4);
}
void kernel_line(const uint8_t *line, uint32_t len) {
    kernel_event(KERNEL_EVENT_LINE, line, len);
}

/* Main loop: plugin first if it listens, else the kernel default. */
static void kernel_event(uint32_t tag, const uint8_t *data, uint32_t len) {
    struct plugin *p = boot_config.plugin;
    if (tag == KERNEL_EVENT_LINE) g_app.nb_lines++;
    if (p && p->event && (p->events & (1 << tag)) && !p->event(tag, data, len)) {
        return;
    }
    switch (tag) {
    case KERNEL_EVENT_INPUT:
        telnet_write_input(&g_app.telnet, data, len);
        break;
    case KERNEL_EVENT_LINE:
        forth_write(data, len);
        break;
    case KERNEL_EVENT_LOG:
        for (uint32_t i = 0; i < len; i++) app_info_putchar(&g_app, data[i]);
        break;
    case KERNEL_EVENT_PLUGIN: {
        uint32_t a;
        mini_memcpy(&a, data, 4);
        plugin_load((const struct plugin_header *)a);
        break;
    }
    }
}

void kqstat(void) {
    LOG("kq events=%d drops=%d lines=%d\n",
        g_app.kq_events, g_app.kq_drops, g_app.nb_lines);
}
void hello(void) {
    LOG("hello!\n");
}
/* Ask update_daemon.sh to push kernel and app.  No ARP client: the
   frame goes to the ethernet broadcast address, so the host must be
   on-link. */
#define UPDATE_PORT 1002
void update(void) {
    struct udp_mon *s = &g_app.udp_mon;
    // udp_mon_reply turns a request into a reply: fake the request
    struct eth_udp p = {
        .mac = { .src_mac = {{0xff,0xff,0xff,0xff,0xff,0xff}}, .dst_mac = s->mac,
                 .ethertype = HTONS(ETHERTYPE_IPV4) },
        .ip  = { .version_ihl = 0x45, .ttl = 64, .protocol = PROTOCOL_UDP,
                 .src_ip = boot_config.update, .dst_ip = s->ip },
        .udp = { .src_port = HTONS(UPDATE_PORT), .dst_port = HTONS(UPDATE_PORT) },
    };
    struct { struct eth_udp eth_udp; uint8_t data[7]; } q;
    mini_memcpy(q.data, "update\n", 7);
    kernel_net_send(&q, udp_mon_reply(&q.eth_udp, &p, sizeof(q.data)));
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
    W(update),     \
    W(kqstat),     \
    W(netstat),    \
    W(oxsend),     \
    W(oxloop),     \
    W(oxrx),       \
    W(lspci),      \
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
/* For sqlite */
void *memmove(void *dest, const void *src, size_t n) {
    uint8_t *d = dest;
    const uint8_t *s = src;
    if (d < s) while (n--) *d++ = *s++;
    else while (n--) d[n] = s[n];
    return dest;
}
int strncmp(const char *s1, const char *s2, size_t n) {
    for (; n && *s1 && *s1 == *s2; n--, s1++, s2++);
    return n ? (uint8_t)*s1 - (uint8_t)*s2 : 0;
}
char *strchr(const char *s, int c) {
    for (; *s != (char)c; s++) if (!*s) return 0;
    return (char *)s;
}
char *strrchr(const char *s, int c) {
    const char *r = 0;
    do { if (*s == (char)c) r = s; } while (*s++);
    return (char *)r;
}
size_t strspn(const char *s, const char *accept) {
    size_t n = 0;
    while (s[n] && strchr(accept, s[n])) n++;
    return n;
}
size_t strcspn(const char *s, const char *reject) {
    size_t n = 0;
    while (s[n] && !strchr(reject, s[n])) n++;
    return n;
}
double fabs(double x) {
    return __builtin_fabs(x);
}




/* Before jumping here, the bootloader loads from media if needed,
   turns off interrupts, switches to protected mode.  The boot_config
   struct is located 512 bytes into the disk or NBP image and is
   loaded at 0x7E00, right after the boot sector at 0x7C00.  Stack is
   set up below 16MB.  A20 is enabled here, see a20_enable().*/


extern uint8_t __bss_start;
extern uint8_t __bss_end;

/* .bss is at 1MB, see kernel.ld.  Without A20 bit 20 is masked:
   clearing .bss would wipe low memory and the kernel.  BIOS int 15h
   2401 failed on real machines (Soekris). */
static int a20_on(void) {
    volatile uint32_t *lo = (void *)0x1000, *hi = (void *)0x101000;
    uint32_t save = *lo;
    *lo = 0x12345678;
    *hi = 0xEDCBA987;
    int on = *lo == 0x12345678;
    *lo = save;
    return on;
}
static void kbc_wait(void) {
    while (inb(0x64) & 2);
}
static int a20_enable(void) {
    if (a20_on()) return 1;
    outb(0x92, (inb(0x92) | 2) & ~1);  // fast A20, bit 0 is reset
    if (a20_on()) return 1;
    kbc_wait(); outb(0x64, 0xD1);      // 8042: write output port
    kbc_wait(); outb(0x60, 0xDF);      // A20 on
    kbc_wait();
    for (int i = 0; i < 100000; i++) if (a20_on()) return 1;
    return 0;
}
extern uint8_t __data_end;

void entry(void) {

    /* Before doing anything, write something to the top right corner
       of the screen to indicate that we got at least this far.  The
       floppy loader writes a ? there before jumping here.  This will
       later be overwritten by the top status line from the
       text_console object. */
    VIDEO[79*2] = '!';

    if (!a20_enable()) {
        // Devices have vga or COM1.  COM1 as the BIOS left it, polled.
        VIDEO[78*2] = 'A';
        for (const char *c = "!A20\r\n"; *c; c++) {
            for (int i = 0; i < 100000; i++) {
                if (inb(0x3F8 + UART_LSR) & LSR_THR_EMPTY) break;
            }
            outb(0x3F8 + UART_DATA, *c);
        }
        for (;;) hlt();
    }

    /* Initialize memory.  */
    mini_memset_volatile(&__bss_start, 0, &__bss_end - &__bss_start);

    /* Initialize hardware and app functionality. */
    app_init(&g_app);

    LOG("version %s\n", VERSION);
    if (0) {
        LOG("%d size\n",  boot_config.endx - boot_config.start);
        LOG("%p start\n", boot_config.start);
        LOG("%p entry\n", boot_config.entry);
        LOG("%p app\n", boot_config.app);
        LOG("%p top\n", boot_config.top);
    }

    /* Start the forth interpreter. */
    forth_start();

    /* Main loop runs the non-real-time work queued by the isrs, with
       interrupts on. */
    struct app *app = &g_app;
    for (;;) {
        cli();
        if (kq_empty(&app->kq)) {
            /* Wait for next interrupt.  sti takes effect after hlt
               starts, so no wakeup is lost.  This is crucial to make
               QEMU more efficient. */
            __asm__ volatile ("sti; hlt");
            continue;
        }
        sti();
        uint16_t tag, len;
        uint8_t *d = kq_peek(&app->kq, &tag, &len);
        if (!d) continue;
        app->kq_events++;
        kernel_event(tag, d, len);
        kq_release(&app->kq);
    }
}


__attribute__ ((section (".config")))
struct boot_config boot_config = {
    .entry = entry,
    .app   = &g_app,
    .top   = &__bss_end,
    .ip    = {{10,1,9,222}},
    .netmask = {{255,255,255,0}},
    .update  = {{10,1,9,1}},
    .endx  = (void*)&__data_end,
    .start = (void*)&boot_config,
};


