/* lwIP plugin, see plugin.h
   - lwIP NO_SYS raw API in the main loop, on udp_mon's mac and ip
   - frames from KERNEL_EVENT_NET, out through kernel_net_send
   - timers on a 50ms kernel_timer_at tick
   - console over TCP port 23: telnet.h per connection, lines go to
     kernel_line, console output comes back through kernel_console_hook
   - http on port 80: httpserver.h serves www/index.html, a console on
     the websocket at /ws (binary frames, one line per message)
   - httpserver.h and websocket.h do blocking reads: here a short read
     longjmps out and the parse reruns when more data arrives.
     Responses are only written once a request was read completely.
   - make lwip_plugin.bin; tether_udp plugin <host> lwip_plugin.bin
   - telnet <host>, http://<host>/ */

#include "plugin.h"
#define LOG(...) kernel_infof(__VA_ARGS__)
// ASSERT on network input (websocket.h) drops the connection
static void conn_abort(void) __attribute__((noreturn));
#define ABORT conn_abort()
#include "tools.h"
#include <string.h>

#define telnet_init_bytes lwip_telnet_init_bytes  // kernel has one too
#include "telnet.h"

#define OS_THREAD_NONE
#include "os_error.h"
static inline const char *os_strerror(os_error_t e) { return ""; }
#include "blocking_io.h"  // before httpserver.h, as in mod_webserver.c
#include "httpserver.h"
#include "sha1.c"

#include "lwip/init.h"
#include "lwip/netif.h"
#include "lwip/etharp.h"
#include "lwip/tcp.h"
#include "lwip/timeouts.h"
#include "netif/ethernet.h"

#define TICK_MS      50
#define TICK_COOKIE  0x6c776970  // "lwip"
#define CONSOLE_PORT 23
#define HTTP_PORT    80
#define NB_CONN      8
#define IN_SIZE      4096
#define OUT_SIZE     16384  // power of 2

static struct netif nif;
static uint8_t nif_mac[6];
static uint8_t tick_pending;  // kernel_timer_at failed, retry on ROOM

/* Console output ring, each connection reads at its own pace */
static struct {
    uint8_t buf[OUT_SIZE];
    uint32_t write;  // free running
} out;
enum conn_kind { CONN_TELNET, CONN_HTTP, CONN_WS };
struct conn {
    struct telnet t;  // first, see conn_of
    struct blocking_io io;
    struct tcp_pcb *pcb;
    uint32_t read;  // in out ring
    enum conn_kind kind;
    /* http and ws input, parsed from the start on each arrival */
    void *jb[5];    // __builtin_setjmp
    uint8_t abort;  // longjmp from conn_abort
    uint32_t in_len, in_pos;
    uint8_t in[IN_SIZE];
    struct webserver_req req;
};
static struct conn conns[NB_CONN];
static struct conn *parsing;  // in http_input
#define conn_of(t) ((struct conn *)(t))
#define conn_of_io(i) ((struct conn *)((uint8_t *)(i) - offsetof(struct conn, io)))
extern const char _binary_www_index_html_start[], _binary_www_index_html_end[];

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
    if (c->kind == CONN_HTTP) return;
    if (out.write - c->read > OUT_SIZE) c->read = out.write - OUT_SIZE;
    while (c->read != out.write) {
        uint32_t o = c->read % OUT_SIZE;
        uint32_t n = out.write - c->read;
        uint32_t room = tcp_sndbuf(c->pcb);
        if (c->kind == CONN_WS) room = room > 4 ? room - 4 : 0;  // frame header
        if (n > OUT_SIZE - o) n = OUT_SIZE - o;  // up to the wrap
        if (n > room) n = room;
        if (!n) break;
        if (c->kind == CONN_WS) {
            struct ws_message m = { .len = n, .buf = out.buf + o, .fin = 1, .opcode = 2 };
            if (ws_write_msg(&c->io, &m)) break;
        }
        else if (tcp_write(c->pcb, out.buf + o, n, TCP_WRITE_FLAG_COPY) != ERR_OK) break;
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

/* http and ws: blocking_io on the input buffer, see top */
static os_error_t io_read(struct blocking_io *io, uint8_t *buf, uintptr_t len) {
    struct conn *c = conn_of_io(io);
    if (c->in_len - c->in_pos < len) __builtin_longjmp(c->jb, 1);
    memcpy(buf, c->in + c->in_pos, len);
    c->in_pos += len;
    return OS_OK;
}
static os_error_t io_write(struct blocking_io *io, const uint8_t *buf, uintptr_t len) {
    struct conn *c = conn_of_io(io);
    return tcp_write(c->pcb, buf, len, TCP_WRITE_FLAG_COPY) == ERR_OK ? OS_OK : os_error(-1);
}
static void in_consume(struct conn *c) {
    memmove(c->in, c->in + c->in_pos, c->in_len - c->in_pos);
    c->in_len -= c->in_pos;
    c->in_pos = 0;
}
static serve_status_t serve_index(struct webserver_req *s) {
    http_write_200_resp(&s->http, "text/html; charset=UTF-8");
    s->http.io->write(s->http.io, (const uint8_t *)_binary_www_index_html_start,
                      _binary_www_index_html_end - _binary_www_index_html_start);
    return WEBSERVER_DONE;
}
static serve_status_t serve_404(struct webserver_req *s) {
    http_write_404_resp(&s->http);
    http_write_str(&s->http, "404 not found\n");
    return WEBSERVER_DONE;
}
static serve_status_t serve_ws(struct webserver_req *s) {
    char key[base64_length(sizeof(s->websocket_sha1)) + 1];
    base64_encode(key, s->websocket_sha1, sizeof(s->websocket_sha1));
    key[sizeof(key) - 1] = 0;
    http_write_str(&s->http, "HTTP/1.1 101 Switching Protocols\r\n"
                   "Upgrade: websocket\r\nConnection: Upgrade\r\n"
                   "Sec-WebSocket-Accept: ");
    http_write_str(&s->http, key);
    http_write_str(&s->http, "\r\n\r\n");
    return WEBSERVER_WEBSOCKET_UP;
}
static http_err_t http_request(struct http_req *h, int method, const char *uri) {
    struct webserver_req *s = (void *)h;
    if      (method != HTTP_METHOD_GET) s->serve = serve_404;
    else if (!strcmp(uri, "/"))         s->serve = serve_index;
    else if (!strcmp(uri, "/ws"))       s->serve = serve_ws;
    else                                s->serve = serve_404;
    return HTTP_ERR_OK;
}
static http_err_t http_header(struct http_req *h, const char *hdr, const char *val) {
    struct webserver_req *s = (void *)h;
    if (!strcmp(hdr, "Sec-WebSocket-Key")) ws_write_sha1(val, s->websocket_sha1);
    return HTTP_ERR_OK;
}
static ws_err_t ws_push(struct blocking_io *io, struct ws_message *m) {
    uint8_t line[TELNET_LINE_BUF_SIZE + 1];
    uint32_t n = m->len < TELNET_LINE_BUF_SIZE ? m->len : TELNET_LINE_BUF_SIZE;
    memcpy(line, m->buf, n);
    line[n] = '\n';
    kernel_line(line, n + 1);
    return WS_OK;
}
static void conn_abort(void) {
    LOG("conn %d abort\n", (int)(parsing - conns));
    parsing->abort = 1;
    __builtin_longjmp(parsing->jb, 1);
}
/* Returns nonzero when the connection is done. */
static int http_input(struct conn *c) {
    parsing = c;
    c->abort = 0;
    if (c->kind == CONN_HTTP) {
        c->in_pos = 0;
        if (__builtin_setjmp(c->jb)) return c->abort;  // short read: wait for more
        memset(&c->req, 0, sizeof(c->req));
        c->req.http.request = http_request;
        c->req.http.header  = http_header;
        c->req.http.io      = &c->io;
        if (http_read_headers(&c->req.http) != HTTP_ERR_OK || !c->req.serve) return 1;
        in_consume(c);
        if (c->req.serve(&c->req) != WEBSERVER_WEBSOCKET_UP) return 1;
        c->kind = CONN_WS;
        c->read = out.write;
        LOG("websocket %d\n", (int)(c - conns));
    }
    for (;;) {
        c->in_pos = 0;
        if (__builtin_setjmp(c->jb)) return c->abort;
        if (ws_read_msg(&c->io, ws_push) != WS_OK) return 1;
        in_consume(c);
    }
}
static err_t conn_recv(void *arg, struct tcp_pcb *pcb, struct pbuf *p, err_t err) {
    struct conn *c = arg;
    if (!p) {
        conn_close(c);
        return ERR_OK;
    }
    // before any close: tcp_close sends RST when data is unacked
    tcp_recved(pcb, p->tot_len);
    if (c->kind == CONN_TELNET) {
        for (struct pbuf *q = p; q; q = q->next) {
            telnet_write_input(&c->t, q->payload, q->len);
        }
    }
    else if (c->in_len + p->tot_len > IN_SIZE) {
        LOG("http input overflow\n");
        pbuf_free(p);
        tcp_abort(pcb);
        c->pcb = 0;
        return ERR_ABRT;
    }
    else {
        pbuf_copy_partial(p, c->in + c->in_len, p->tot_len, 0);
        c->in_len += p->tot_len;
        if (http_input(c)) conn_close(c);
    }
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
static err_t conn_accept(void *arg, struct tcp_pcb *pcb, err_t err) {
    struct conn *c = 0;
    for (int i = 0; i < NB_CONN; i++) if (!conns[i].pcb) { c = &conns[i]; break; }
    if (err != ERR_OK || !c) {
        tcp_abort(pcb);
        return ERR_ABRT;
    }
    c->pcb = pcb;
    c->read = out.write;
    c->kind = (enum conn_kind)(uintptr_t)arg;
    c->in_len = c->in_pos = 0;
    c->io.read = io_read;
    c->io.write = io_write;
    tcp_arg(pcb, c);
    tcp_recv(pcb, conn_recv);
    tcp_sent(pcb, conn_sent);
    tcp_err(pcb, conn_err);
    if (c->kind == CONN_TELNET) {
        telnet_init(&c->t, conn_write, conn_event);  // sends telnet_init_bytes
        LOG("tcp console %d\n", (int)(c - conns));
    }
    return ERR_OK;
}
static void listen(uint16_t port, enum conn_kind kind) {
    struct tcp_pcb *pcb = tcp_new();
    tcp_bind(pcb, IP_ADDR_ANY, port);
    pcb = tcp_listen(pcb);
    tcp_arg(pcb, (void *)kind);
    tcp_accept(pcb, conn_accept);
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

    listen(CONSOLE_PORT, CONN_TELNET);
    listen(HTTP_PORT, CONN_HTTP);

    kernel_console_hook = console_hook;
    tick();
    LOG("lwip %d.%d.%d.%d ports %d %d\n", ip[0], ip[1], ip[2], ip[3], CONSOLE_PORT, HTTP_PORT);
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
