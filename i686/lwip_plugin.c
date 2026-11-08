/* lwIP plugin, see plugin.h
   - lwIP NO_SYS raw API in the main loop, on udp_mon's mac and ip
   - frames from KERNEL_EVENT_NET, out through kernel_net_send
   - timers on a 50ms kernel_timer_at tick
   - console over TCP port 23: telnet.h per connection, lines go to
     kernel_line, console output comes back through kernel_console_hook
   - make lwip_plugin.bin; tether_udp plugin <host> lwip_plugin.bin
   - telnet <host> */

#include "plugin.h"
#define LOG(...) kernel_infof(__VA_ARGS__)
#include "tools.h"
#include <string.h>

#define telnet_init_bytes lwip_telnet_init_bytes  // kernel has one too
#include "telnet.h"

#include "lwip/init.h"
#include "lwip/netif.h"
#include "lwip/etharp.h"
#include "lwip/tcp.h"
#include "lwip/timeouts.h"
#include "netif/ethernet.h"

#define TICK_MS      50
#define TICK_COOKIE  0x6c776970  // "lwip"
#define CONSOLE_PORT 23
#define NB_CONN      4
#define OUT_SIZE     16384  // power of 2

static struct netif nif;
static uint8_t nif_mac[6];
static uint8_t tick_pending;  // kernel_timer_at failed, retry on ROOM

/* Console output ring, each connection reads at its own pace */
static struct {
    uint8_t buf[OUT_SIZE];
    uint32_t write;  // free running
} out;
struct conn {
    struct telnet t;  // first, see conn_of
    struct tcp_pcb *pcb;
    uint32_t read;
};
static struct conn conns[NB_CONN];
#define conn_of(t) ((struct conn *)(t))

u32_t sys_now(void) {
    return kernel_ms;
}
int atoi(const char *s) {  // netif_find
    int n = 0;
    while (*s >= '0' && *s <= '9') n = 10 * n + *s++ - '0';
    return n;
}

/* Network interface */
static err_t nif_linkoutput(struct netif *n, struct pbuf *p) {
    static uint8_t f[1514];
    if (p->tot_len > sizeof(f)) return ERR_BUF;
    pbuf_copy_partial(p, f, p->tot_len, 0);
    return kernel_net_send(f, p->tot_len) ? ERR_IF : ERR_OK;
}
static err_t nif_init(struct netif *n) {
    n->name[0] = 'u';
    n->name[1] = 'm';
    n->mtu = 1500;
    n->hwaddr_len = 6;
    memcpy(n->hwaddr, nif_mac, 6);
    n->flags = NETIF_FLAG_BROADCAST | NETIF_FLAG_ETHARP | NETIF_FLAG_ETHERNET;
    n->output = etharp_output;
    n->linkoutput = nif_linkoutput;
    return ERR_OK;
}
static void net_rx(const uint8_t *data, uint32_t len) {
    struct pbuf *p = pbuf_alloc(PBUF_RAW, len, PBUF_POOL);
    if (!p) return;
    pbuf_take(p, data, len);
    if (nif.input(p, &nif) != ERR_OK) pbuf_free(p);
}

/* Console */
static void out_putchar(uint8_t c) {
    out.buf[out.write++ % OUT_SIZE] = c;
}
static void console_hook(uint8_t c) {
    if (c == '\n') out_putchar('\r');
    out_putchar(c);
}
static void conn_flush(struct conn *c) {
    if (out.write - c->read > OUT_SIZE) c->read = out.write - OUT_SIZE;
    while (c->read != out.write) {
        uint32_t o = c->read % OUT_SIZE;
        uint32_t n = out.write - c->read;
        if (n > OUT_SIZE - o) n = OUT_SIZE - o;  // up to the wrap
        if (n > tcp_sndbuf(c->pcb)) n = tcp_sndbuf(c->pcb);
        if (!n || tcp_write(c->pcb, out.buf + o, n, TCP_WRITE_FLAG_COPY) != ERR_OK) break;
        c->read += n;
    }
    tcp_output(c->pcb);
}
static void flush(void) {
    for (int i = 0; i < NB_CONN; i++) if (conns[i].pcb) conn_flush(&conns[i]);
}
/* Echo and telnet negotiation, this connection only */
static void conn_write(struct telnet *t, const uint8_t *buf, uintptr_t len) {
    struct conn *c = conn_of(t);
    if (c->pcb) tcp_write(c->pcb, buf, len, TCP_WRITE_FLAG_COPY);
}
static void conn_event(struct telnet *t, uintptr_t event) {
    if ((event & ~0xff) != TELNET_EVENT_LINE) return;
    uint8_t line[TELNET_LINE_BUF_SIZE + 1];
    memcpy(line, t->line, t->nb_char);
    line[t->nb_char] = '\n';  // forth_accept() wants white space
    kernel_line(line, t->nb_char + 1);
}
static void conn_close(struct conn *c) {
    tcp_arg(c->pcb, 0);
    tcp_recv(c->pcb, 0);
    tcp_sent(c->pcb, 0);
    tcp_err(c->pcb, 0);
    if (tcp_close(c->pcb) != ERR_OK) tcp_abort(c->pcb);
    c->pcb = 0;
}
static err_t conn_recv(void *arg, struct tcp_pcb *pcb, struct pbuf *p, err_t err) {
    struct conn *c = arg;
    if (!p) {
        conn_close(c);
        return ERR_OK;
    }
    for (struct pbuf *q = p; q; q = q->next) {
        telnet_write_input(&c->t, q->payload, q->len);
    }
    tcp_recved(pcb, p->tot_len);
    pbuf_free(p);
    return ERR_OK;
}
static err_t conn_sent(void *arg, struct tcp_pcb *pcb, u16_t len) {
    conn_flush(arg);
    return ERR_OK;
}
static void conn_err(void *arg, err_t err) {
    struct conn *c = arg;
    c->pcb = 0;  // already freed
}
static err_t console_accept(void *arg, struct tcp_pcb *pcb, err_t err) {
    struct conn *c = 0;
    for (int i = 0; i < NB_CONN; i++) if (!conns[i].pcb) { c = &conns[i]; break; }
    if (err != ERR_OK || !c) {
        tcp_abort(pcb);
        return ERR_ABRT;
    }
    c->pcb = pcb;
    c->read = out.write;
    telnet_init(&c->t, conn_write, conn_event);  // sends telnet_init_bytes
    tcp_arg(pcb, c);
    tcp_recv(pcb, conn_recv);
    tcp_sent(pcb, conn_sent);
    tcp_err(pcb, conn_err);
    LOG("tcp console %d\n", (int)(c - conns));
    return ERR_OK;
}

/* Events */
static void tick(void) {
    tick_pending = !!kernel_timer_at(kernel_ms + TICK_MS, TICK_COOKIE);
}
static int lwip_event(uint32_t tag, const uint8_t *data, uint32_t len) {
    switch (tag) {
    case KERNEL_EVENT_NET:
        net_rx(data, len);
        break;
    case KERNEL_EVENT_TIMER: {
        const struct kernel_timer_event *e = (const void *)data;
        if (e->cookie != TICK_COOKIE) return -1;
        sys_check_timeouts();
        tick();
        break;
    }
    case KERNEL_EVENT_ROOM:
        if (tick_pending) tick();
        return -1;  // others may wait for it too
    default:
        return -1;
    }
    flush();
    return 0;
}
static struct plugin lwip_plugin = {
    .name   = "lwip",
    .events = (1 << KERNEL_EVENT_NET) | (1 << KERNEL_EVENT_TIMER) |
              (1 << KERNEL_EVENT_ROOM),
    .event  = lwip_event,
};
static struct plugin *lwip_plugin_init(void) {
    lwip_init();
    uint8_t ip[4];
    kernel_net_addr(nif_mac, ip);
    ip4_addr_t a, mask, gw;
    IP4_ADDR(&a, ip[0], ip[1], ip[2], ip[3]);
    IP4_ADDR(&mask, 255, 255, 255, 0);
    IP4_ADDR(&gw, 0, 0, 0, 0);
    netif_add(&nif, &a, &mask, &gw, 0, nif_init, ethernet_input);
    netif_set_default(&nif);
    netif_set_up(&nif);
    netif_set_link_up(&nif);

    struct tcp_pcb *pcb = tcp_new();
    tcp_bind(pcb, IP_ADDR_ANY, CONSOLE_PORT);
    pcb = tcp_listen(pcb);
    tcp_accept(pcb, console_accept);

    kernel_console_hook = console_hook;
    tick();
    LOG("lwip %d.%d.%d.%d port %d\n", ip[0], ip[1], ip[2], ip[3], CONSOLE_PORT);
    return &lwip_plugin;
}

extern uint8_t __plugin_bss_start[], __plugin_bss_end[];
__attribute__((section(".plugin_header")))
const struct plugin_header plugin_header = {
    .magic          = PLUGIN_MAGIC,
    .kernel_version = KERNEL_VERSION,
    .bss_start      = __plugin_bss_start,
    .bss_end        = __plugin_bss_end,
    .init           = lwip_plugin_init,
};
