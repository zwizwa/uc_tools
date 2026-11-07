#ifndef MOD_UDP_MON
#define MOD_UDP_MON

#include "pbuf.h"

/* Output buffer is always flushed after interpreting a single packet,
   so a pbuf works better than a cbuf. */
#define MONITOR_3IF_OUT_BUF_T     struct pbuf
#define MONITOR_3IF_OUT_BUF_PUT   pbuf_put
#define MONITOR_3IF_OUT_BUF_CLEAR pbuf_clear

#include "mod_monitor_3if.c"
#include "crc.h"
#include "uct_byteswap.h"
#include "ethernet.h"
#include "ethernet_rx.h"

/* Minimalistic monitor setup over UDP.
   Main point is to be able to reload the kernel without reboot.

   Keep this end as simple as possible and handle complexity at the
   host end using a dedicated tool.

   - static IP
   - ARP
   - ping for debugging
   - can run from interrupt
   - uc_tools sm.h style state machine
*/

#define UDP_MON_CONSOLE_PORT 1001
#define UDP_MON_COMMAND_PORT  799 // 0x31f
#define UDP_MON_LOG_SIZE     4096 // power of 2
#define UDP_MON_LOG_CHUNK    1024


typedef void (*udp_mon_send_fn)(void *ctx, const uint8_t *data, uint32_t len);

struct udp_mon {

    /* OS interface. */
    void (*send)(void *ctx, const uint8_t *data, uint32_t len);
    void (*key)(void *ctx, uint8_t byte);
    int  (*log_send)(void *ctx, const uint8_t *data, uint32_t len); // nonzero: tx full
    void *ctx;

    /* Our mac and ip addresses */
    struct mac_addr mac;
    struct ip_addr  ip;

    /* The monitor state.
       There should be no reliance on state between 2 udp packets
       unless there was an ack. */
    struct monitor_3if monitor_3if;

    /* Console output
       - ring collects from boot, oldest bytes are overwritten
       - goes to the last host that wrote to the console port
       - log_peer holds that host's request headers */
    struct eth_udp log_peer;
    uint8_t log_connected;
    uint32_t log_read, log_write; // free running
    uint8_t log_buf[UDP_MON_LOG_SIZE];

};

/* Turn request headers into reply headers, returns packet size. */
static inline uint32_t udp_mon_reply(struct eth_udp *q, const struct eth_udp *p, uint32_t data_len) {
    /* Start it out with the same fields as the request. */
    *q = *p;

    struct udp *udp = &q->udp;
    udp->dst_port = p->udp.src_port;
    udp->src_port = p->udp.dst_port;
    uint16_t l = sizeof(struct udp) + data_len;
    udp->length = HTONS(l);
    udp->checksum = 0; // Not needed on IPv4

    struct ip *ip = &q->ip;
    ip->dst_ip = p->ip.src_ip;
    ip->src_ip = p->ip.dst_ip;
    l += sizeof(struct ip);
    ip->total_length = HTONS(l);
    ip->header_checksum = 0;
    ip->header_checksum = ip_checksum(ip, sizeof(*ip));

    struct mac *mac = &q->mac;
    mac->dst_mac = p->mac.src_mac;
    mac->src_mac = p->mac.dst_mac;
    l += sizeof(struct mac);
    return l;
}

/* Call with interrupts off.  When tx is full the rest stays in the
   ring until the next flush. */
static inline void udp_mon_log_flush(struct udp_mon *s) {
    if (!s->log_connected) return;
    struct {
        struct eth_udp eth_udp;
        uint8_t data[UDP_MON_LOG_CHUNK];
    } q;
    while (s->log_read != s->log_write) {
        uint32_t n = s->log_write - s->log_read;
        if (n > UDP_MON_LOG_CHUNK) n = UDP_MON_LOG_CHUNK;
        for (uint32_t i=0; i<n; i++) {
            q.data[i] = s->log_buf[(s->log_read + i) % UDP_MON_LOG_SIZE];
        }
        uint32_t l = udp_mon_reply(&q.eth_udp, &s->log_peer, n);
        if (s->log_send(s->ctx, (uint8_t*)&q, l)) return;
        s->log_read += n;
    }
}

/* Call with interrupts off. */
static inline void udp_mon_log_putchar(struct udp_mon *s, uint8_t c) {
    s->log_buf[s->log_write++ % UDP_MON_LOG_SIZE] = c;
    if (s->log_write - s->log_read > UDP_MON_LOG_SIZE) {
        s->log_read = s->log_write - UDP_MON_LOG_SIZE;
    }
    if (c == '\n') udp_mon_log_flush(s);
}

// socat - UDP:10.1.54.222:1001
// socat - STDIO,raw,echo=0,escape=0x03 UDP:10.1.54.222:1001
static inline void udp_mon_rx_udp(struct udp_mon *s, const uint8_t *eth_pkt, uint32_t len) {
    const struct eth_udp *p = (const void*)eth_pkt;
    // FIXME: checksum
    int32_t data_len = HTONS(p->udp.length) - sizeof(struct udp);
    if (sizeof(*p) + data_len > len) {
        LOG("udp bad data_len=%d\n", data_len);
        return;
    }
    const uint16_t port = HTONS(p->udp.dst_port);
    const uint8_t *data = (const void*)&p[1];
    // LOG("udp port=%d data_len=%d\n", port, data_len);
    // log_hex(data, data_len);
    if (port == UDP_MON_CONSOLE_PORT) {
        s->log_peer = *p;
        s->log_connected = 1;
        for (int32_t i=0; i<data_len; i++) {
            uint8_t c = data[i];
            if (c == 0xa) { c = 0xd; }
            s->key(s->ctx, c);
        }
        udp_mon_log_flush(s);
    }
    else if (port == UDP_MON_COMMAND_PORT) {
        /* Run the 3if commands in the packet, record the replies in a
           pbuf and send a reply with the same sequence number.  The
           sequence number is intentionally only 8 bit so a small
           array of handlers can be used at the host end. */
        /* Last command is {5, BCK, crc32 LE} over the rest, see
           tether_udp_send_packet.  Drop on mismatch: no ack, so the
           host retransmits.  Otherwise it runs as a normal BCK. */
        if (data_len < 6 || data[data_len-6] != 5 || data[data_len-5] != BCK ||
            crc32b(data, data_len-6) != read_le(data + data_len-4, 4)) {
            LOG("udp_mon bad crc\n");
            return;
        }
        uint32_t buf_size = 1472;
        struct {
            struct eth_udp eth_udp;
            uint8_t data[buf_size];
        } q;

        /* Set up UDP packet body as reply buffer */
        struct pbuf pbuf;
        pbuf_init(&pbuf, q.data, buf_size);
        s->monitor_3if.out = &pbuf;
        /* Push all opcodes into the 3if state machine */
        // log_hex(data, data_len);
        for (int32_t i=0; i<data_len; i++) {
            monitor_3if_push_key(&s->monitor_3if, data[i]);
        }
        /* Disconnect out buffer */
        s->monitor_3if.out = NULL;
        // log_hex(pbuf.buf, pbuf.count);

        uint32_t l = udp_mon_reply(&q.eth_udp, p, pbuf.count);
        s->send(s->ctx, (uint8_t*)&q, l);
    }
}

/* Called from interrupt context. */
static inline void udp_mon_rx(struct udp_mon *s, const uint8_t *data, uint32_t len) {
    const struct mac *e = (const void*)data;
    if (len < sizeof(*e)) return;
    uint16_t ethertype = NTOHS(e->ethertype);

    if (0) {
        //LOG("udp_mon rx: %p %p %p\n", s, s->send, s->ctx);
        LOG("udp_mon rx %d ", len);
        log_mac(&e->dst_mac); LOG(" ");
        log_mac(&e->src_mac); LOG(" ");
        LOG("%04x", ethertype); LOG("\n");
    }

    /* FIXME: validate checksums */
    switch(ethertype) {
    case ETHERTYPE_ARP:
        arp_rx(s->send, s->ctx, data, len, &s->ip, &s->mac);
        break;
    case ETHERTYPE_IPV4: {
        const struct {
            struct mac mac;
            struct ip  ip;
        } *p = (void*)data;
        if (p->ip.version_ihl != 0x45) {
            // We assume no options in the header.
            LOG("ipv4 unsupported version_ihl=0x%02x\n", p->ip.version_ihl);
            break;
        }
        // LOG("ipv4 protocol %d\n", p->ip.protocol);
        switch(p->ip.protocol) {
        case PROTOCOL_ICMP:
            // peer is gone, and logging this would feed back into it
            if (((const struct icmp *)&p[1])->type == ICMP_DEST_UNREACHABLE) {
                s->log_connected = 0;
            }
            icmp_rx(s->send, s->ctx, data, len, &s->ip);
            break;
        case PROTOCOL_UDP:
            udp_mon_rx_udp(s, data, len);
            break;
        default:
            break;
        }
        //udp_mon_rx_udp(s, data, len);
        break;
    }
    default:
        break;
    }
}

static inline void udp_mon_init(struct udp_mon *s,
                                udp_mon_send_fn send,
                                void (*key)(void *, uint8_t),
                                void *ctx) {
    //LOG("udp_mon_init %p %p %p\n", s, send, ctx);
    // No memset: .bss is clear and the log ring is already collecting.
    s->send = send;
    s->key  = key;
    s->ctx  = ctx;
    // Push it once with dummy input to boot the state machine.
    monitor_3if_push_key(&s->monitor_3if, 0);

}



#endif
