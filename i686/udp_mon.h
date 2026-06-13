#ifndef UDP_MON_H
#define UDP_MON_H

#include "ethernet.h"


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


typedef void (*udp_mon_send_fn)(void *ctx, const uint8_t *data, uint32_t len);

struct udp_mon {
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

#define CONSOLE_UDP_PORT 1234

// FIXME: hardcoded
struct app;
extern struct app g_app;
void app_keyboard_input(struct app *app, uint8_t byte);

// socat - UDP:10.1.3.222:1234
// socat - STDIO,raw,echo=0,escape=0x03 UDP:10.1.3.222:1234
static inline void udp_mon_rx_udp(struct udp_mon *s, const uint8_t *eth_pkt, uint32_t len) {
    const struct {
        struct mac mac;
        struct ip ip;
        struct udp udp;
    } *p = (const void*)eth_pkt;
    // FIXME: checksum
    uint32_t data_len = HTONS(p->udp.length) - sizeof(struct udp);
    if (sizeof(*p) + data_len > len) return;
    const uint16_t port = HTONS(p->udp.d_port);
    const uint8_t *data = (const void*)&p[1];
    // LOG("udp port=%d data_len=%d\n", port, data_len);
    // log_hex(data, data_len);
    if (port == CONSOLE_UDP_PORT) {
        for (uint32_t i=0; i<data_len; i++) {
            uint8_t c = data[i];
            if (c == 0xa) { c = 0xd; }
            app_keyboard_input(&g_app, c);
        }
    }
    //else if (port == s->udp_mon_port) {
    //}
}

/* Called from interrupt context. */
static inline void udp_mon_rx(struct udp_mon *s, const uint8_t *data, uint32_t len) {
    const struct mac *e = (const void*)data;
    if (len < sizeof(*e)) return;
    uint16_t ethertype = NTOHS(e->ethertype);

    if (0) {
        //LOG("udp_mon rx: %p %p %p\n", s, s->send, s->ctx);
        LOG("udp_mon rx %d ", len);
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

static inline void udp_mon_tick(struct udp_mon *s) {
    if (s->next) goto *s->next;
    //LOG("udp_mon init\n");

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


static inline void udp_mon_init(struct udp_mon *s, udp_mon_send_fn send, void *ctx) {
    LOG("udp_mon_init %p %p %p\n", s, send, ctx);
    memset(s, 0, sizeof(*s));
    s->send = send;
    s->ctx = ctx;
    udp_mon_tick(s);
}



#endif
