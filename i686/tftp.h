#ifndef TFTP_H
#define TFTP_H

#include "ethernet.h"


/* Minimalistic tftp setup for kernel reload without reboot.
   - static IP
   - ARP
   - ping for debugging
   - can run from interrupt
   - uc_tools sm.h style state machine
*/

static inline void log_mac(const uint8_t *mac) {
    for(int i=0; i<6; i++) {
        if (i>0) LOG(":");
        LOG("%02x", mac[i]);
    }
}
static inline void log_ipv4(const uint8_t *mac) {
    for(int i=0; i<4; i++) {
        if (i>0) LOG(".");
        LOG("%d", mac[i]);
    }
}


typedef void (*tftp_send_fn)(void *ctx, const uint8_t *data, uint32_t len);

struct tftp {
    void *next;

    /* System */
    void *ctx;
    void (*send)(void *ctx, const uint8_t *data, uint32_t len);

    uint8_t *buffer;
    uint32_t length;

    /* We only talk to the server */
    uint8_t server_ip[4];
    uint8_t server_mac[6];

    uint8_t mac[6];
    uint8_t ip[4];

};

// socat - UDP:10.1.3.222:1234
static inline void tftp_rx_udp(struct tftp *s, const uint8_t *eth_pkt, uint32_t len) {
    const struct {
        struct mac mac;
        struct ip ip;
        struct udp udp;
    } *p = (const void*)eth_pkt;
    // FIXME: checksum
    uint32_t data_len = HTONS(p->udp.length) - sizeof(struct udp);
    if (sizeof(*p) + data_len > len) return;
    LOG("udp data_len=%d\n", data_len);
    const uint8_t *data = (const void*)&p[1];
    log_hex(data, data_len);
}

/* Called from interrupt context. */
static inline void tftp_rx(struct tftp *s, const uint8_t *data, uint32_t len) {
    const struct mac *e = (const void*)data;
    if (len < sizeof(*e)) return;
    uint16_t ethertype = NTOHS(e->ethertype);

    if (1) {
        //LOG("tftp rx: %p %p %p\n", s, s->send, s->ctx);
        LOG("tftp rx %d ", len);
        log_mac(e->d_mac); LOG(" ");
        log_mac(e->s_mac); LOG(" ");
        LOG("%04x", ethertype); LOG("\n");
    }

    /* FIXME: validate checksums */
    switch(ethertype) {
    case ETHERTYPE_ARP:
        arp_rx(s->send, s->ctx, data, len, s->ip, s->mac);
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
        switch(p->ip.protocol) {
        case PROTOCOL_ICMP:
            icmp_rx(s->send, s->ctx, data, len, s->ip);
            break;
        case PROTOCOL_UDP:
            tftp_rx_udp(s, data, len);
            break;
        default:
            break;
        }
        //tftp_rx_udp(s, data, len);
        break;
    }
    default:
        break;
    }
}

static inline void tftp_tick(struct tftp *s) {
    if (s->next) goto *s->next;
    //LOG("tftp init\n");

    // send arp request for 10.1.3.1
    // wait for reply
    // send udp
    // wait reply
    // etc...
    // arp reply is handled in the background

  loop:
    s->next = &&loop;
    return;
}


static inline void tftp_init(struct tftp *s, tftp_send_fn send, void *ctx) {
    LOG("tftp_init %p %p %p\n", s, send, ctx);
    memset(s, 0, sizeof(*s));
    s->send = send;
    s->ctx = ctx;
    tftp_tick(s);
}



#endif
