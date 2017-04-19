#include "rsp_packet.h"

/* Building RSP packets */

// 1. checksummed $...#xx format
int32_t rsp_begin(struct packet *rpl) {
    return packet_save(rpl, '+')
        || packet_save(rpl, '$');
}
int32_t rsp_end(struct packet *rpl) {
    return packet_save(rpl, '#')
        || packet_save_hex_cs(rpl, rpl->checksum);  // _cs : don't care - already got it
}
// 2. OK
int32_t rsp_OK(struct packet *rpl) {
    return rsp_begin(rpl)
        || packet_save_cs(rpl, 'O')
        || packet_save_cs(rpl, 'K')
        || rsp_end(rpl);
}
// 3. Exx
int32_t rsp_E(struct packet *rpl, uint8_t e) {
    return rsp_begin(rpl)
        || packet_save_cs(rpl, 'E')
        || packet_save_hex_cs(rpl, e)
        || rsp_end(rpl);
}
// 4. Monitor
int32_t rsp_hex_cstring(struct packet *p, const char *cstring) {
    int32_t e;
    if ((e = rsp_begin(p))) return e;
    for (uint32_t i = 0; cstring[i]; i++) {
        if ((e = packet_save_hex_cs(p, cstring[i]))) return e;
    }
    return rsp_end(p);
}
// 5. Decode
static int32_t ok_is_busy(uint32_t e) {
    return e == E_OK ? E_BUSY : e;
}
int32_t rsp_decode_putchar(struct packet *p, uint8_t c) {
    if (!p->state) p->state = &&start;

    //LOG("%c",c);
    goto *p->state;

  start:
    packet_init(p);
    p->state = &&wait;
    goto wait;

  wait:
    if (c == 3)   { LOG("break\n");       return E_BUSY; }
    if (c == '-') { LOG("nack\n");        return E_BUSY; }
    if (c == '+') { /*LOG("ack\n");*/     return E_BUSY; }
    if (c == '$') { p->state = &&collect; return E_BUSY; }
    LOG("unexpected 0x%02x\n", c); {
        /* Abort in case we see something we don't know.  This is used
           upstream to switch command consoles. */
        return E_ABORT;
    }

  collect:
    //LOG("collect %c\n", c);
    if (c == '#') { p->state = &&check1;  return E_BUSY; }
    packet_update_checksum(p, c);

    if (c == '*') { p->state = &&rle;    return E_BUSY; }
    if (c == '}') { p->state = &&escape; return E_BUSY; }
    else return ok_is_busy(packet_save(p, c));

  check1:
    p->state = &&check2;
    p->expected <<= 4; p->expected |= hex_char2int_ignore(c);
    return E_BUSY;

  check2:
    p->state = &&start;
    p->expected <<= 4; p->expected |= hex_char2int_ignore(c);
    return E_OK; // Packet is done, ready for interpretation.

  escape:
    p->state = &&collect;
    packet_update_checksum(p, c);
    return ok_is_busy(packet_save(p, c ^ 0x20));

  rle:
    p->state = &&collect;
    packet_update_checksum(p, c);
    int32_t count = c - 28 - 1;
    if (p->wr < 1) return E_PROTO;
    uint8_t fill = p->buf[p->wr-1];
    while(count--) {
        int32_t err = packet_save(p, fill);
        if (err) return err;
    }
    return E_BUSY;
}


// Read hex word, LSD first, up to separator character, and skip over
// the separator.  This is to avoid scanf and the deps it pulls in.
static inline void skip_one(const uint8_t **pc, uint32_t *plen) {
    (*pc)++;
    (*plen)--;
}
int32_t take_hex(const uint8_t **pc, uint32_t *plen, uint32_t *pval) {
    int32_t digit;
    uint32_t nb_digits = 0;
    *pval = 0;
    while((digit = hex_char2int_check(**pc)) >= 0) {
        //LOG("digit %c %d\n", **pc, digit);
        *pval = ((*pval) << 4) | digit;
        skip_one(pc,plen);
        nb_digits++;
    }
    skip_one(pc,plen);
    return nb_digits ? 0 : E_PROTO;
}
