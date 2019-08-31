#include "cbuf.h"

/* Slip-aware decode baseband byte or out-of-band (CBUF_OOB) code.  It
 * seems simplest to implement this as a peek into a circular
 * buffer .*/
uint16_t cbuf_peek_slip_decode(struct cbuf *b, uint32_t *nb_drop) {
    uint16_t fc = cbuf_peek(b, 0);
    if (CBUF_EAGAIN == fc){
        /* Nothing in buffer. */
        *nb_drop = 0;
        return fc;
    }
    if (SLIP_END == fc) {
        /* Sync byte. */
        *nb_drop = 1;
        return CBUF_OOB(SLIP_END);
    }
    if (SLIP_ESC == fc) {
        fc = cbuf_peek(b, 1);
        if (CBUF_EAGAIN == fc) {
            /* Incomplete escape code.  For the caller, this is the
             * same as nothing in buffer. */
            *nb_drop = 0;
            return CBUF_EAGAIN;
        }
        *nb_drop = 2;
        /* Undo escape. */
        if (SLIP_ESC_END == fc) return SLIP_END;
        if (SLIP_ESC_ESC == fc) return SLIP_ESC;
        /* Anything else is mapped to the out-of-band range. Note that
         * a SLIP_ESC SLIP_END sequence results in CBUF_OOB(SLIP_END),
         * which is what it should be.  Such a sequence is meaningless
         * and probably indicates lost bytes. */
        return CBUF_OOB(fc);
    }
    *nb_drop = 1;
    return fc;
}


/* Implement read in terms of peek + drop. */
uint16_t cbuf_get_slip_decode(struct cbuf *b) {
    uint32_t nb_drop = 0;
    uint16_t fc = cbuf_peek_slip_decode(b, &nb_drop);
    cbuf_drop(b, nb_drop);
    return fc;
}




/* SLIP-encode an OOB-encode fat character. */
void cbuf_put_slip(struct cbuf *b, uint16_t fc) {
    if (fc > 0xFF) {
        if (fc == CBUF_OOB(SLIP_END)) {
            cbuf_put(b, SLIP_END);
        }
        else if (0x200 == (fc & 0xF00)) {
            /* Any byte other than SLIP_END would be valid. */
            cbuf_put(b, SLIP_ESC);
            cbuf_put(b, fc & 0xFF);
        }
        else {
            /* Just ignore invalid fat chars */
        }
    }
    else if (fc == SLIP_END) {
        cbuf_put(b, SLIP_ESC);
        cbuf_put(b, SLIP_ESC_END);
    }
    else if (fc == SLIP_ESC) {
        cbuf_put(b, SLIP_ESC);
        cbuf_put(b, SLIP_ESC_ESC);
    }
    else {
        cbuf_put(b, fc);
    }
}
/* Write a complete double-ended slip packet. */
void cbuf_write_slip(struct cbuf *b, uint8_t *buf, uint32_t len) {
    cbuf_put_slip(b, CBUF_OOB(SLIP_END));
    for(uint32_t i=0; i<len; i++) {
        cbuf_put_slip(b, buf[i]);
    }
    cbuf_put_slip(b, CBUF_OOB(SLIP_END));
}
