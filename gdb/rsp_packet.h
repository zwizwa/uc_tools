#ifndef RSP_PACKET_H
#define RSP_PACKET_H

#include "generic.h"
#include "tools.h"
#include <stdint.h>
#include <string.h>

#ifndef LOG
#define LOG(...)
#endif

/* GDB RSP packet parsing routines + misc tools. */

#define E_OK       0  // packet is ready
#define E_PROTO   -1  // protocol parse error
#define E_BUF     -2  // buffer full error
#define E_MEM     -3  // memory access error
#define E_ABORT   -4  // abort gdb stub / switch IO protocol
#define E_BUSY    -5  // no error, but packet isn't done yet
#define E_TIMEOUT -6  // fixme: standard code?
#define E_NAK     -7  //



int32_t take_hex(const uint8_t **pc, uint32_t *plen, uint32_t *pval);

// packet object
// data is stored in this way:
// - input state machine unpacks
// - output state machine packs

struct packet {
    uint8_t *buf;
    uint32_t wr;
    uint32_t rd;
    uint32_t size;
    void *state;  // parser state
    uint8_t checksum;
    uint8_t expected;
};

static inline void packet_init(struct packet *p) {
    p->wr = 0;
    p->rd = 0;
    p->checksum = 0;
    p->state = 0;
    if (p->buf) memset(p->buf,0,p->size); // zero-termination is used
}
// Save a control character - do not update checksum.
static inline int32_t packet_save(struct packet *p, uint8_t c) {
    // No buf -> don't save.
    // Used for just delineating in rs485 adapter.
    if (!p->buf) return E_OK;

    if (p->wr < p->size) {
        p->buf[p->wr++] = c;
        return E_OK;
    }
    else {
        return E_BUF;
    }
}

static inline int32_t packet_update_checksum(struct packet *p, uint8_t c) {
    p->checksum += c;
    return E_OK;
}

static inline int32_t packet_save_cs(struct packet *p, uint8_t c) {
    return packet_save(p, c)
        || packet_update_checksum(p, c);
}

static inline int32_t packet_save_string_cs(struct packet *p, const char *str) {
    while(*str) {
        int32_t rv = packet_save_cs(p, *str++);
        if (rv) return rv;
    }
    return E_OK;
}


static inline uint32_t packet_save_hex_cs(struct packet *p, uint8_t b) {
    uint8_t hex[2];
    write_hex_nibbles(hex, b, 2);
    return packet_save_cs(p,hex[0])
        || packet_save_cs(p,hex[1]);
}
static inline uint32_t packet_save_u32_hex_cs(struct packet *p, uint32_t w) {
    return packet_save_hex_cs(p, w)
        || packet_save_hex_cs(p, w >> 8)
        || packet_save_hex_cs(p, w >> 16)
        || packet_save_hex_cs(p, w >> 24);
}

static inline int32_t packet_save_nstring_cs(struct packet *p, const char *str, uint32_t n) {
    while(n--) {
        int32_t rv = packet_save_cs(p, *str++);
        if (rv) return rv;
    }
    return E_OK;
}


static inline uint32_t packet_save_escaped_cs(struct packet *p, uint8_t c) {
    switch(c) {
    case '#':
    case '$':
    case '}':
    case '*':
        return packet_save_cs(p, '}')
            || packet_save_cs(p, c ^ 0x20);
    default:
        return packet_save_cs(p, c);
    }
}



int32_t rsp_begin(struct packet *rpl);
int32_t rsp_end(struct packet *rpl);
int32_t rsp_OK(struct packet *rpl);
int32_t rsp_E(struct packet *rpl, uint8_t e);
int32_t rsp_hex_cstring(struct packet *p, const char *cstring);

int32_t rsp_decode_putchar(struct packet *p, uint8_t c);


#endif
