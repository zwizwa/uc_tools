/* Logic Analyzer WebGL prototype

   Overview of the la_gl files; the others point here.

   Data flow, 8x1 samples at 2Msps (one byte per sample, bit n = port n):

     sigrok.sh (Logic8)  --stdin-->  la_gl.c  --http/ws-->  la_gl.html
     test_la_gl_dmx.c (fake DMX)       |
                                       +-- <buffer>      ring of raw samples
                                       +-- <buffer>.d/   minmax mipmap levels

   Files
   - la_gl.c: this server, derived from la_ws.c and la_dmx.c
   - la_gl.sh: launcher, pipes a stream program into la_gl.c
     default sigrok.sh, or pass test_la_gl_dmx.dynamic.host.elf
   - sigrok.sh: Logic8 via sigrok-cli fx2lafw, raw binary on stdout
   - test_la_gl_dmx.c: fake 8 port DMX at 2Msps, real time
   - webapp/la_gl/la_gl.html: webgl scope + packet list, single page

   la_gl.c, main thread (la_loop)
   - read one slice (2^20 samples) into the mmap ring, nb_slices long
   - run 8 la_uart DMX decoders over it, frames go to pkt[] log
   - update minmax levels 8..20 for that slice (mod_minmax_8x1.c)
   - advance la_time; view lags by one slice (0.5s)

   la_gl.c, http thread + one thread per client
   - serves documentroot, websocket on the same port
   - la_lock guards map, la_time and pkt[] between both sides

   Websocket protocol, ad-hoc binary, little endian
   - request: struct req, 24 bytes
   - CMD_WINDOW: left, width at level -> struct rpl_window
     one minmax pair per column, min=AND max=OR of 2^level samples
     index wraps around the ring, below level 8 computed from raw
   - CMD_PACKETS: since seq -> struct rpl_packets, last PKT_NB only

   View (la_gl.html)
   - fragment shader decodes 8 channels from one min/max texel/column
   - wheel zoom, drag pan, space toggles live, click packet to jump

   Run on psamathe (no gcc there; build in sandbox, same glibc)
   - systemd-run --unit=la_gl -E PATH=/run/current-system/sw/bin \
       /exo/deps/uc_tools/linux/la_gl.sh
   - http://10.1.9.1:8091/, ring in /var/lib/la_gl/

   Limits
   - not in rules.mk, built by hand with linux/build.sh
   - one file per mipmap level, not a single second file
   - plain js webgl, not gl/mod_uct_gl.c; protocol not tag_u32 */

#include "os_thread.h"
#include <signal.h>
os_mutex_t la_lock;
#define LA_EXCLUSIVE() \
    WITH_PRE_POST(&la_lock, os_mutex_lock, os_mutex_unlock)

#define WEBSERVER_INDEX_HTML "la_gl.html"

#include "mod_minmax_8x1.c"
#include "mod_webserver.c"
#include "sha1.c"

struct minmax_map map;
uintptr_t nb_slices;

/* Nb of samples in completed slices.  Not wrapped. */
uint64_t la_time;


/* ANALYZER */

struct uart_out;
static void frame_out(struct uart_out *s);
#include "mod_la_dmx.c"

#define FOR_PORTS(m) \
    m(0) m(1) m(2) m(3) \
    m(4) m(5) m(6) m(7) \

#define PUSH(n) \
    la_uart_push(&s_##n, &e);

FOR_PORTS(DEF_PORT)

/* Circular packet log, indexed by sequence number. */
#define PKT_NB   256
#define PKT_DATA 32
struct pkt {
    uint64_t time;  // break, in samples
    uint32_t seq;
    uint16_t port;
    uint16_t count; // slots received, data[] is truncated
    uint8_t  data[PKT_DATA];
};
struct pkt pkt[PKT_NB];
uint32_t pkt_seq;

static void frame_out(struct uart_out *s) {
    /* Break after idle flush. */
    if (!s->count) return;
    LA_EXCLUSIVE() {
        struct pkt *p = &pkt[pkt_seq % PKT_NB];
        memset(p, 0, sizeof(*p));
        p->time  = s->brk_time;
        p->seq   = pkt_seq++;
        p->port  = s->port;
        p->count = s->count;
        for (int i=0; i<s->count && i<PKT_DATA; i++) {
            p->data[i] = s->buf[i];
        }
    }
}


/* VIEW */

#define CMD_WINDOW  0
#define CMD_PACKETS 1

struct req {
    uint32_t cmd;
    uint32_t arg;   // window: level, packets: first seq
    int64_t  left;  // window: left edge, in samples at level
    uint32_t width; // window: nb of minmax pairs
    uint32_t pad;
};
struct rpl_window {
    uint32_t cmd;
    uint32_t level;
    int64_t  left;
    uint64_t time;  // samples recorded
    uint64_t size;  // samples in circular buffer
    struct minmax_minmax mm[];
};
struct rpl_packets {
    uint32_t cmd;
    uint32_t nb;
    struct pkt pkt[];
};

static void rpl(struct blocking_io *io, void *buf, uintptr_t len) {
    struct ws_message m = { .opcode = 2, .fin = 1, .buf = buf, .len = len };
    ws_write_msg(io, &m);
}

static void rpl_window(struct blocking_io *io, const struct req *req) {
    uint32_t level = req->arg, w = req->width;
    if (level > MINMAX_LEVEL_ENDI) return;
    if (w > 8192) return;
    uint8_t buf[sizeof(struct rpl_window) + w * sizeof(struct minmax_minmax)];
    struct rpl_window *r = (void*)buf;
    r->cmd   = CMD_WINDOW;
    r->level = level;
    r->left  = req->left;
    r->size  = nb_slices * MINMAX_SLICE_SIZE;
    /* The buffer is circular at every level. */
    off_t nb = minmax_nb_at_level(&map, level);
    LA_EXCLUSIVE() {
        r->time = la_time;
        for (uint32_t i=0; i<w; i++) {
            off_t abs = (req->left + i) % nb;
            if (abs < 0) abs += nb;
            r->mm[i] = (level < map.level_start) ?
                minmax_compute(minmax_original(&map), abs, level) :
                minmax_level(&map, level)[abs];
        }
    }
    rpl(io, buf, sizeof(buf));
}

static void rpl_packets(struct blocking_io *io, const struct req *req) {
    uint8_t buf[sizeof(struct rpl_packets) + sizeof(pkt)];
    struct rpl_packets *r = (void*)buf;
    r->cmd = CMD_PACKETS;
    r->nb  = 0;
    LA_EXCLUSIVE() {
        uint32_t seq = req->arg;
        if (pkt_seq - seq > PKT_NB) seq = pkt_seq - PKT_NB;
        while (seq != pkt_seq) {
            r->pkt[r->nb++] = pkt[seq++ % PKT_NB];
        }
    }
    rpl(io, buf, sizeof(*r) + r->nb * sizeof(struct pkt));
}

ws_err_t websocket_push(struct blocking_io *io, struct ws_message *m) {
    struct req req;
    if (m->len != sizeof(req)) return 0;
    memcpy(&req, m->buf, sizeof(req));
    switch (req.cmd) {
    case CMD_WINDOW:  rpl_window(io, &req);  break;
    case CMD_PACKETS: rpl_packets(io, &req); break;
    }
    return 0;
}

/* One thread per connection.  webserver_loop() is sequential and
   stalls on the idle spare connections that Chromium opens. */
static void *client_loop(void *ctx) {
    struct os_tcp_socket *socket = ctx;
    struct webserver_req req = {};
    if (WEBSERVER_WEBSOCKET_UP == server_serve(&req, &socket->io)) {
        while (WS_OK == ws_read_msg(&socket->io, websocket_push));
    }
    os_tcp_done(socket);
    free(socket);
    return NULL;
}
OS_THREAD_STACK(http_thread, 1024);
uint16_t tcp_port = 0;
OS_THREAD_MAIN(http_loop, ctx) {
    struct os_tcp_server server;
    os_tcp_server_init(&server, tcp_port);
    for (;;) {
        struct os_tcp_socket *socket = malloc(sizeof(*socket));
        ASSERT(socket);
        if (OS_OK != os_tcp_accept(&server, socket)) { free(socket); continue; }
        pthread_t thread;
        ASSERT(0 == pthread_create(&thread, NULL, client_loop, socket));
        pthread_detach(thread);
    }
    return 0;
}


/* RECORDER */

int la_loop(void) {
    uint8_t *circ_buf = minmax_original(&map);
    uintptr_t slice_nb = 0;
    struct la_event e = {};
    for(;;) {
        /* Written without locking: the view only looks at completed
           slices, except for the one being overwritten. */
        uint8_t *buf = circ_buf + MINMAX_SLICE_SIZE * slice_nb;
        assert_read_fixed(0, buf, MINMAX_SLICE_SIZE);

        for (uintptr_t i=0; i<MINMAX_SLICE_SIZE; i++) {
            e.value = buf[i];
            FOR_PORTS(PUSH)
            e.time++;
        }
        LA_EXCLUSIVE() {
            minmax_update_slice(&map, slice_nb);
            la_time += MINMAX_SLICE_SIZE;
        }
        slice_nb = (slice_nb + 1) % nb_slices;
    }
    return 0;
}

int main(int argc, char **argv) {
    if (argc != 5) {
        LOG("usage: %s <documentroot> <buffer> <nb_slices> <tcp_port>\n", argv[0]);
        return 1;
    }
    nb_slices = atoi(argv[3]);
    tcp_port = atoi(argv[4]);
    minmax_open_buf(&map, argv[2], 8, nb_slices);

    ASSERT_ERRNO(chdir(argv[1]));
    signal(SIGPIPE, SIG_IGN);
    OS_THREAD_START(http_thread, http_loop, NULL);

    return la_loop();
}
