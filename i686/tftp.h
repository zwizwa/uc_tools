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

/* This handles ping and ignores everything else.  */
static inline void tftp_rx_icmp(struct tftp *s, const uint8_t *data, uint32_t len) {
    const struct {
        struct mac mac;
        struct ip ip;
        struct icmp icmp;
    } *p = (const void*)data;
    if (memcmp(p->ip.d_ip, s->ip, 4)) return;
    if (p->icmp.type == ICMP_PING) {
        LOG("ping from "); log_ipv4(p->ip.s_ip); LOG("\n");
    }
    else {
        LOG("icmp %02x %02x\n", p->icmp.type, p->icmp.code);
    }
}

/* Called from internet context.  This handles arp.  */
static inline void tftp_rx_arp(struct tftp *s, const uint8_t *data, uint32_t len) {
    const struct mac *mac = (const void*)data;
    const struct arp *arp = (const void*)(data + sizeof(*mac));
    if (len < sizeof(*arp)) return;

    if (arp->htype != HTONS(ARP_HTYPE_ETH) ||
        arp->ptype != HTONS(ARP_PTYPE_IPV4) ||
        arp->hlen  != ETH_ALEN ||
        arp->plen  != 4) {
        LOG("bad arp\n");
        return;
    }
    uint16_t op = HTONS(arp->oper);

    switch (op) {
    case ARP_OP_REQUEST: {
        // who has arp->tpa tell arp->spa
        LOG("who has "); log_ipv4(arp->tpa);
        LOG(" tell ");   log_ipv4(arp->spa); LOG("\n");
        if (!memcmp(arp->tpa, s->ip, 4)) {
            struct {
                struct mac mac;
                struct arp arp;
            } reply = {};
            LOG("i have\n");
            memcpy(reply.mac.d_mac, mac->s_mac, 6);
            memcpy(reply.mac.s_mac, s->mac, 6);
            reply.mac.ethertype = htons(ETHERTYPE_ARP);
            reply.arp.htype = HTONS(ARP_HTYPE_ETH);
            reply.arp.ptype = HTONS(ARP_PTYPE_IPV4);
            reply.arp.hlen  = ETH_ALEN;
            reply.arp.plen  = 4;
            reply.arp.oper  = HTONS(ARP_OP_REPLY);
            memcpy(reply.arp.sha, s->mac, 6);
            memcpy(reply.arp.spa, s->ip, 4);
            s->send(s->ctx, (const uint8_t*)&reply, sizeof(reply));
            // I got a ping after this, so seems that linux accepts it.
        }
        break;
    }
    case ARP_OP_REPLY:
        break;
    default:
        return;
    }


}
static inline void tftp_rx_udp(struct tftp *s, const uint8_t *data, uint32_t len) {
}

/* Called from internet context. */
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

    /* FIXME: Check destination. Only respond to broadcast and mac.
       Currently assume that card will filter out other packets. */
    if (ETHERTYPE_ARP == ethertype) {
        tftp_rx_arp(s, data, len);
    }
    else if (ETHERTYPE_ICMP == ethertype) {
        tftp_rx_icmp(s, data, len);
    }
    else if (0) {
        tftp_rx_udp(s, data, len);
    }
    else {
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
