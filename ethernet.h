#ifndef ETHERNET_H
#define ETHERNET_H

// FIXME: actually make these platform independent

#define NTOHS(w) ((((w)&0xFF00) >> 8) | (((w)&0x00FF) << 8))
#define HTONS(w) NTOHS(w)

#define NTOHL(w) (NTOHS((w)>>16) | (NTOHS(w)<<16))
#define HTONL(w) NTOHL(w)

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


struct __attribute__((packed)) mac {
    uint8_t d_mac[6];
    uint8_t s_mac[6];
    uint16_t ethertype;
    // uint32_t checksum follows payload
} ;
struct __attribute__((packed)) ip {
    uint8_t version_ihl;
    uint8_t dscp_ecn;
    uint16_t total_length;
    uint16_t identification;
    uint16_t flags_fo;
    uint8_t ttl;
    uint8_t protocol;
    uint16_t header_checksum;
    uint8_t s_ip[4];
    uint8_t d_ip[4];
    // options if IHL>5
} ;
struct __attribute__((packed)) udp {
    uint16_t s_port;
    uint16_t d_port;
    uint16_t length;
    uint16_t checksum;
};





#endif
