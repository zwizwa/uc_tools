#ifndef PCAP_WRITE_H
#define PCAP_WRITE_H

// https://wiki.wireshark.org/Development/LibpcapFileFormat

// native ordering can be used.  it is detected based on magic
#define PCAP_WRITE_MAGIC 0xa1b2c3d4

#pragma align(1)
struct pcap_write_hdr {
    uint32_t magic_number;   /* magic number */
    uint16_t version_major;  /* major version number */
    uint16_t version_minor;  /* minor version number */
    int32_t  thiszone;       /* GMT to local correction */
    uint32_t sigfigs;        /* accuracy of timestamps */
    uint32_t snaplen;        /* max length of captured packets, in octets */
    uint32_t network;        /* data link type */
};

static inline void pcap_write_hdr_init(struct pcap_write_hdr *h) {
    h->magic_number = PCAP_WRITE_MAGIC;
    h->version_major = 2;
    h->version_minor = 4;
    h->thiszone = 0;
    h->sigfigs = 0;
    h->snaplen = 65535;
    h->network = 1; // Ethernet
}

#pragma align(1)
struct struct pcap_write_rec {
    uint32_t ts_sec;         /* timestamp seconds */
    uint32_t ts_usec;        /* timestamp microseconds */
    uint32_t incl_len;       /* number of octets of packet saved in file */
    uint32_t orig_len;       /* actual length of packet */
};



#enif
