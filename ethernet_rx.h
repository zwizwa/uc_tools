#ifndef ETHERNET_RX_H
#define ETHERNET_RX_H

#include "ethernet.h"

/* Some handlers for minimal implementation to get UDP going. */
// Move into #include "ethernet_rx.h"

typedef void (*packet_send_fn)(void *ctx,
                               const uint8_t *data,
                               uint32_t len);

static inline void arp_rx(
    packet_send_fn send, void *ctx,
    const uint8_t *data, uint32_t len,
    const struct ip_addr *my_ip,
    const struct mac_addr *my_mac)
{
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
        // LOG("who has "); log_ipv4(arp->tpa);
        // LOG(" tell ");   log_ipv4(arp->spa); LOG("\n");
        if (ip_eq(&arp->tpa, my_ip)) {
            struct {
                struct mac mac;
                struct arp arp;
            } reply = {};
            // LOG("i have\n")
            reply.mac.dst_mac = mac->src_mac;
            reply.mac.src_mac = mac->dst_mac;
            reply.mac.ethertype = htons(ETHERTYPE_ARP);
            reply.arp.htype = HTONS(ARP_HTYPE_ETH);
            reply.arp.ptype = HTONS(ARP_PTYPE_IPV4);
            reply.arp.hlen  = ETH_ALEN;
            reply.arp.plen  = 4;
            reply.arp.oper  = HTONS(ARP_OP_REPLY);
            reply.arp.sha = *my_mac;
            reply.arp.spa = *my_ip;
            send(ctx, (const uint8_t*)&reply, sizeof(reply));
        }
        break;
    }
    case ARP_OP_REPLY:
        break;
    default:
        return;
    }


}

static inline void icmp_rx(
    packet_send_fn send, void *ctx,
    const uint8_t *data, uint32_t len,
    const struct ip_addr *my_ip)
{
    // FIXME: This should use the ip header length field, beause len
    // is ethernet length which is padded.  It happens to work on
    // linux.
    uint32_t data_len =
        len - sizeof(struct mac) - sizeof(struct ip) - sizeof(struct icmp);
    struct {
        struct mac mac;
        struct ip ip;
        struct icmp icmp;
        uint8_t data [data_len];
    } reply[1], *q = &reply[0], *p = ((void*)data);
    if (p->icmp.type != ICMP_ECHO_REQUEST) {
        LOG("icmp %02x %02x\n", p->icmp.type, p->icmp.code);
        return;

    }
    // FIXME: Also support broadcast reply
    if (!ip_eq(&p->ip.dst_ip, my_ip)) return;
    // LOG("ping from "); log_ipv4(p->ip.s_ip); LOG("\n");
    memcpy(q, data, len);
    q->mac.dst_mac = p->mac.src_mac;
    q->mac.src_mac = p->mac.dst_mac;
    q->ip.dst_ip = p->ip.src_ip;
    q->ip.src_ip = p->ip.dst_ip;
    q->icmp.type = ICMP_ECHO_REPLY;
    q->ip.header_checksum = 0; // zero before computing the checksum
    q->ip.header_checksum = ip_checksum(&q->ip, sizeof(q->ip));
    uint32_t icmp_len = ((void*)&reply[1]) - ((void*)&q->icmp);
    q->icmp.checksum = 0;
    q->icmp.checksum = ip_checksum(&q->icmp, icmp_len);
    send(ctx, (const uint8_t*)q, len);
}



#endif
