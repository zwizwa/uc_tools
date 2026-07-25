#ifndef PERSISTENT_SRAM_H
#define PERSISTENT_SRAM_H

#include "crc.h"
#include "log_hex.h"

/* Put some data outside of the memory that is initialized at boot to
   allow inforamtion to be passed between reboots of a controller.

   Operations:
   - Seal  = compute CRC checksum and padding
   - Check = verify CRC checksum and padding

   A note on dealing with random contents:

   Initial state of SRAM on power-on is a combination of sate
   determined by size mismatch in the individual inverters in the SRAM
   and noise for tranistors that are mathed well.  See this paper
   which talks about using SRAM for fingerprinting and entropy
   generation

   https://archiv.infsec.ethz.ch/education/as09/secsem/papers/SRAM.pdf

   What I take from this is that there is a really high chance that
   some cells predictably power up as 1, such that a "zero check" can
   greatly reduce the chance of a false positive.
*/


#define PRAM_OK            0
#define PRAM_BAD_SLOT_SIZE 1
#define PRAM_BAD_DATA_LEN  2
#define PRAM_BAD_ZERO_FILL 3
#define PRAM_BAD_SLOT_CRC  4

#define PRAM_TYPE_CUSTOMIZE 1
#define PRAM_TYPE_CODE      2

/* This header file does not know _where_ the data is stored.
   Typically it would be stored somewhare above the end of the bss,
   e.g. at the very top of SRAM, such that the crc in param_footer is
   tha last u32 in the sram. */

struct pram_meta {
    uint32_t slot_size;
    uint32_t data_len;
    uint32_t type;
    uint32_t crc; // last word in slot, top of sram
};

/* Handle */
struct pram_slot {
    uint8_t *data; // start of slot has the payload data
    struct pram_meta *meta;
};

#ifndef PRAM_LOG
#define PRAM_LOG(...)
#endif

static inline int pram_check(struct pram_slot *s,     /* valid if return value is PRAM_OK */
                             uint8_t *slot_min_start, /* e.g. derived from bss end */
                             uint8_t *slot_endx)      /* e.g. top of sram */
{
    s->meta = (void*)slot_endx - sizeof(struct pram_meta);
    const struct pram_meta *m = s->meta;

    /* Check slot size */
    uint32_t max_slot_size = slot_endx - slot_min_start;
    if ((m->slot_size < sizeof(*m)) ||
        (m->slot_size > max_slot_size)) {
        return PRAM_BAD_SLOT_SIZE;
    }
    s->data = (void*)(slot_endx - m->slot_size);

    /* Check data size */
    if (m->data_len > (max_slot_size - sizeof(struct pram_meta))) {
        return PRAM_BAD_DATA_LEN;
    }

    /* Check zero fill */
    int32_t zero_fill = m->slot_size - m->data_len - sizeof(*m);
    if (zero_fill > 0) {
        PRAM_LOG("checking zerofill %d: ", zero_fill);
        for (uint32_t i=0; i<zero_fill; i++) {
            uint8_t b = s->data[m->data_len + i];
            PRAM_LOG(" %02x", b);
            if (b != 0) {
                PRAM_LOG("\n");
                return PRAM_BAD_ZERO_FILL;
            }
        }
        PRAM_LOG("\n");
    }
    else {
        PRAM_LOG("not checking zerofill\n");
    }

    /* Check CRC */
    uint32_t crc_size = m->slot_size-4;
    PRAM_LOG("crc32 %p %d\n", s->data, crc_size);
    uint32_t crc = crc32b(s->data, crc_size);
    PRAM_LOG("crc %08x, expected %08x\n", crc, m->crc);
    // log_hex(s->data, crc_size);

    if (m->crc != crc) {
        return PRAM_BAD_SLOT_CRC;
    }

    // FIXME: Zero pad test?

    /* All good */
    return PRAM_OK;
}


static inline void pram_seal(uint32_t type,
                             void *data,
                             uint32_t data_len,
                             uint32_t slot_size)
{
    /* Caller needs to put data in the correct place such that
       data_slot size == SRAM endx.  And data_len needs to fit.  We
       don't check any of these here. */
    struct pram_meta *m = (void*)(data + slot_size - sizeof(*m));
    m->data_len = data_len;
    m->slot_size = slot_size;
    m->type = type;
    int32_t zero_fill = slot_size - data_len - sizeof(*m);
    if (zero_fill > 0) {
        PRAM_LOG("zero fill %d bytes at %p\n",
                 zero_fill, data + data_len);
        memset(data + data_len, 0, zero_fill);
    }
    m->crc = crc32b(data, slot_size-4);

}

#endif
