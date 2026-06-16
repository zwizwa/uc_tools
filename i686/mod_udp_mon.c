#ifndef MOD_UDP_MON
#define MOD_UDP_MON

#include "pbuf.h"

/* Output buffer is always flushed after interpreting a single packet,
   so a pbuf works better than a cbuf. */
#define MONITOR_3IF_OUT_BUF_T     struct pbuf
#define MONITOR_3IF_OUT_BUF_PUT   pbuf_put
#define MONITOR_3IF_OUT_BUF_CLEAR pbuf_clear

#include "mod_monitor_3if.c"
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


typedef void (*udp_mon_send_fn)(void *ctx, const uint8_t *data, uint32_t len);

struct udp_mon {

    /* OS interface. */
    void (*send)(void *ctx, const uint8_t *data, uint32_t len);
    void (*key)(void *ctx, uint8_t byte);
    void *ctx;

    /* Our mac and ip addresses */
    struct mac_addr mac;
    struct ip_addr  ip;

    /* The monitor state.
       There should be no reliance on state between 2 udp packets
       unless there was an ack. */
    struct monitor_3if monitor_3if;

};


// socat - UDP:10.1.3.222:1234
// socat - STDIO,raw,echo=0,escape=0x03 UDP:10.1.3.222:1234
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
        for (int32_t i=0; i<data_len; i++) {
            uint8_t c = data[i];
            if (c == 0xa) { c = 0xd; }
            s->key(s->ctx, c);
        }
    }
    else if (port == UDP_MON_COMMAND_PORT) {
        /* Run the 3if commands in the packet, record the replies in a
           pbuf and send a reply with the same sequence number.  The
           sequence number is intentionally only 8 bit so a small
           array of handlers can be used at the host end. */
        uint32_t buf_size = 1472;
        struct {
            struct eth_udp eth_udp;
            uint8_t data[buf_size];
        } q;
        /* Start it out with the same fields as the request. */
        q.eth_udp = *p;

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

        struct udp *udp = &q.eth_udp.udp;
        udp->dst_port = p->udp.src_port;
        udp->src_port = p->udp.dst_port;
        uint16_t l = sizeof(struct udp) + pbuf.count;
        udp->length = HTONS(l);
        udp->checksum = 0; // Not needed on IPv4

        struct ip *ip = &q.eth_udp.ip;
        ip->dst_ip = p->ip.src_ip;
        ip->src_ip = p->ip.dst_ip;
        l += sizeof(struct ip);
        ip->total_length = HTONS(l);
        ip->header_checksum = 0;
        ip->header_checksum = ip_checksum(ip, sizeof(*ip));

        struct mac *mac = &q.eth_udp.mac;
        mac->dst_mac = p->mac.src_mac;
        mac->src_mac = p->mac.dst_mac;
        l += sizeof(struct mac);
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
    LOG("udp_mon_init %p %p %p\n", s, send, ctx);
    memset(s, 0, sizeof(*s));
    s->send = send;
    s->key  = key;
    s->ctx  = ctx;
    // Push it once with dummy input to boot the state machine.
    monitor_3if_push_key(&s->monitor_3if, 0);

}



#endif
