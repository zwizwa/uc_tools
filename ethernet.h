#ifndef ETHERNET_H
#define ETHERNET_H


// FIXME: actually make these platform independent

#include "uct_byteswap.h"
#if 0
#define NTOHS(w) ((((w)&0xFF00) >> 8) | (((w)&0x00FF) << 8))
#define HTONS(w) NTOHS(w)
#define NTOHL(w) (NTOHS((w)>>16) | (NTOHS(w)<<16))
#define HTONL(w) NTOHL(w)
#endif

static inline uint16_t ntohs(uint16_t w) { return NTOHS(w); }
static inline uint16_t htons(uint16_t w) { return HTONS(w); }
static inline uint32_t ntohl(uint32_t w) { return NTOHL(w); }
static inline uint32_t htonl(uint32_t w) { return HTONL(w); }

static inline uint16_t ip_checksum(const void *vdata, size_t length) {

    const uint8_t *data = vdata;
    // Initialise the accumulator.
    uint32_t acc = 0xffff;
    // Handle complete 16-bit blocks.
    for (size_t i = 0; i+1 < length; i += 2) {
        uint16_t word;
        memcpy(&word, data + i, 2);
        acc += ntohs(word);
        if (acc > 0xffff) {
            acc -= 0xffff;
        }
    }
    // Handle any partial block at the end of the data.
    if (length & 1) {
        uint16_t word = 0;
        memcpy(&word, data + length - 1, 1);
        acc += ntohs(word);
        if (acc > 0xffff) {
            acc -= 0xffff;
        }
    }
    // Return the checksum in network byte order.
    return htons(~acc);

}

// FIXME: add checksum computations for headers.
// FIXME: one's complement checksums are endian independent (is this true?)
// https://www.quora.com/Why-is-it-that-UDP-takes-the-1s-complement-of-the-sum-that-is-why-not-just-use-the-sum

// http://www.microhowto.info/howto/calculate_an_internet_protocol_checksum_in_c.html

#define ETHERTYPE_ARP  0x0806
#define ETHERTYPE_IPV4 0x0800
#define PROTOCOL_UDP       17
#define PROTOCOL_ICMP       1

// Wrap the addresses in a struct so C assignment can be used.
struct __attribute__((packed)) mac_addr {
    uint8_t mac[6];
};
struct __attribute__((packed)) ip_addr {
    uint8_t ip[4];
};

#define DEF_STRUCT_EQ(fun_name, struct_name)                    \
    static inline int fun_name(const struct struct_name *a,     \
                               const struct struct_name *b) {   \
    return !memcmp(a,b,sizeof(*a)); }

DEF_STRUCT_EQ(ip_eq, ip_addr);
DEF_STRUCT_EQ(mac_eq, mac_addr);


struct __attribute__((packed)) mac {
    struct mac_addr dst_mac;
    struct mac_addr src_mac;
    uint16_t ethertype;
    // uint32_t checksum follows payload
} ;

#define ETH_ALEN        6
#define ARP_HTYPE_ETH   0x0001
#define ARP_PTYPE_IPV4  0x0800
#define ARP_OP_REQUEST  0x0001
#define ARP_OP_REPLY    0x0002

struct __attribute__((packed)) arp {
    uint16_t htype;              /* hardware type        */
    uint16_t ptype;              /* protocol type        */
    uint8_t  hlen;               /* hardware addr length */
    uint8_t  plen;               /* protocol addr length */
    uint16_t oper;               /* operation            */
    struct mac_addr sha;         /* sender hardware addr */
    struct ip_addr  spa;         /* sender protocol addr */
    struct mac_addr tha;         /* target hardware addr */
    struct ip_addr  tpa;         /* target protocol addr */
};
struct __attribute__((packed)) ip {
    uint8_t version_ihl;
    uint8_t dscp_ecn;
    uint16_t total_length;
    uint16_t identification;
    uint16_t flags_fo;
    uint8_t ttl;
    uint8_t protocol;
    uint16_t header_checksum;
    struct ip_addr src_ip;
    struct ip_addr dst_ip;
    // options if IHL>5
};
#define ICMP_ECHO_REQUEST 8
#define ICMP_ECHO_REPLY   0
struct __attribute__((packed)) icmp {
    uint8_t type;
    uint8_t code;
    uint16_t checksum;
};

struct __attribute__((packed)) udp {
    uint16_t src_port;
    uint16_t dst_port;
    uint16_t length;
    uint16_t checksum;
};

// Complete ethernet + ip + protocol headers */
struct __attribute__((packed)) eth_udp {
    struct mac mac;
    struct ip  ip;
    struct udp udp;
};



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
