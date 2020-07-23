#ifndef PCAP_WRITE_H
#define PCAP_WRITE_H

#include <stdint.h>
#include "assert_read.h"

// https://wiki.wireshark.org/Development/LibpcapFileFormat

// native ordering can be used.  it is detected based on magic
#define PCAP_MAGIC 0xa1b2c3d4

#pragma pack(1)
struct pcap_hdr {
    uint32_t magic_number;   /* magic number */
    uint16_t version_major;  /* major version number */
    uint16_t version_minor;  /* minor version number */
    int32_t  thiszone;       /* GMT to local correction */
    uint32_t sigfigs;        /* accuracy of timestamps */
    uint32_t snaplen;        /* max length of captured packets, in octets */
    uint32_t network;        /* data link type */
};
void assert_read_pcap_hdr(int fd, struct pcap_hdr *h) {
    assert_read_fixed(fd, h, sizeof(*h));
    if (PCAP_MAGIC != h->magic_number) {
        ERROR("PCAP_MAGIC %x != %x\n", PCAP_MAGIC, h->magic_number);
    };
    ASSERT_EQ(h->version_major, 2);
    ASSERT_EQ(h->version_minor, 4);
    ASSERT_EQ(h->network, 1);
}

static inline void pcap_hdr_init(struct pcap_hdr *h) {
    h->magic_number = PCAP_MAGIC;
    h->version_major = 2;
    h->version_minor = 4;
    h->thiszone = 0;
    h->sigfigs = 0;
    h->snaplen = 65535;
    h->network = 1; // Ethernet
};

#pragma pack(1)
struct pcap_rec {
    uint32_t ts_sec;         /* timestamp seconds */
    uint32_t ts_usec;        /* timestamp microseconds */
    uint32_t incl_len;       /* number of octets of packet saved in file */
    uint32_t orig_len;       /* actual length of packet */
    uint8_t data[0];
};

static inline struct pcap_rec *assert_read_pcap_rec(int fd, void *buf, size_t len) {
    struct pcap_rec *r = buf;

    ASSERT(len >= sizeof(*r));
    assert_read_fixed(fd, r, sizeof(*r));
    ASSERT(len >= sizeof(*r) + r->incl_len);
    assert_read_fixed(fd, r+1, r->incl_len);
    return r;
}





#endif
