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



// FIXME: This should probably abort it the fat character doesn't fit.

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
    cbuf_put(b, SLIP_END);
    for(uint32_t i=0; i<len; i++) {
        cbuf_put_slip(b, buf[i]);
    }
    cbuf_put(b, SLIP_END);
}


void cbuf_write_slip_slices(struct cbuf *b, const struct slice *slice, uint32_t n_slices) {
    cbuf_put(b, SLIP_END);
    for(uint32_t s=0; s<n_slices; s++) {
        const uint8_t *buf = slice[s].buf;
        for(uint32_t i=0; i<slice[s].len; i++) {
            cbuf_put_slip(b, buf[i]);
        }
    }
    cbuf_put(b, SLIP_END);
}


int cbuf_write_slip_from_read(
    struct cbuf *b,
    const uint8_t *hdr, uint32_t hdr_len,
    uint32_t buf_size,
    uint32_t (*read)(uint8_t *buf, uint32_t len)) {
    /* What's a good size?  This is guaranteed to fit if the cbuf is
       empty, but there might be other messages appended before it can
       be drained out completely.  All in all this is not a good
       mechanism. */
    uint8_t buf[buf_size];
    uint32_t n = read(buf, sizeof(buf));
    if (!n) return 0;
    const struct slice slices[] = {
        {.buf = hdr, .len = hdr_len},
        {.buf = buf, .len = n},
    };
    cbuf_write_slip_slices(b, slices, 2);
    return 1;
}

#if 0

// This is a bad interface, because there is no peek.

/* The thing I end up with all the time is a need for a routine that
   spills a chunk from a stream into a cbuf, slip-tagged.  This is a
   bit involved because it's hard to predict the size.  So we're going
   to rely on unwinding. */
int cbuf_write_slip_header_stream(
    struct cbuf *b,
    uint8_t *header, uint32_t header_len,
    uint32_t (*read)(uint8_t *buf, uint32_t len)) {

    uint32_t write0 = b->write;

    /* To simplify code we're going to assume the unknown next
       character is fat.  cbuf_put_slip() might otherwise write a
       partial fat character. */
    cbuf_put(b, SLIP_END);
    for(uint32_t i=0; i<header_len; i++) {
        if (cbuf_room(b) < 2) goto abort;
        cbuf_put_slip(b, header[i]);
    }
    while ((cbuf_room(b) >= 3) && (read(&buf, 1))) {
        /* There is room for one more character plus terminator.  We
           can't call abort here so make sure it fits. */
        cbuf_put_slip(b, buf);
    }
    cbuf_put(b, SLIP_END);
    return 1;

  abort:
    b->write = write0;
    return 0;
}

#endif


void cbuf_write_slip_tagged(struct cbuf *b, uint16_t tag,
                            const uint8_t *buf, uint32_t len) {
    uint8_t tagbuf[] = {tag >> 8, tag & 0xff};
    struct slice slices[] = {
        {tagbuf, sizeof(tagbuf)},
        {buf, len}
    };
    cbuf_write_slip_slices(b, slices, 2);
}
