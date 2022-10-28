#ifndef FWSTREAM_H
#define FWSTREAM_H

#include <stdint.h>
#include "macros.h"
#include "gdbstub_api.h"

/* Decoder for firmware image stream. */

/* Note that it would be nice to share code with partition_config.h
   partition_config_valid() since it does pretty much the same thing,
   but that other code is a bootloader and is not intended to change
   over time.  This code here is intended to evolve over time. */
#include "partition_config.h"


/* Firmware blocksize is hardcoded to STM32F103 erase block size.
   FIXME: Metadata needs to be extended to support different block
   sizes. */
#define BLOCK_LOGSIZE 10
#define BLOCK_SIZE    (1 << BLOCK_LOGSIZE)

#define FWSTREAM_OK 0
#define FWSTREAM_ERR_GAP 1
#define FWSTREAM_ERR_FW_ENDX 2
#define FWSTREAM_ERR_FW_SIZE 3
#define FWSTREAM_ERR_CTRL_SIZE 4
#define FWSTREAM_ERR_CTRL_CRC 5
#define FWSTREAM_ERR_FW_CRC 6
#define FWSTREAM_ERR_NOT_WRITTEN 7
#define FWSTREAM_ERR_BAD_REPORT 8

struct fwstream;

struct fwstream {

    /* CONFIG */

    /* Memory write callback. */
    const uint8_t* (*write)(struct fwstream *, uintptr_t rel_chunk, const uint8_t *chunk_data);
    uintptr_t chunk_size; // It's simpler to keep this constant.
    uintptr_t max_size;   // Used as consistency check

    /* Incremental checksum computation of written and re-read data
       using e.g. crc32b. */
    uint32_t (*checksum_inc)(const uint8_t *, uint32_t len, uint32_t acc);

    /* Priority to set before writing the control block to flash. */
    uint32_t priority;


    /* STATE */

    uint32_t checksum_acc;

    /* Iteration state.  The checksum is byte-aligned. */
    uintptr_t start_addr;
    uintptr_t endx_addr;

    /* First chunk past the firmware is the control block. */
    uintptr_t control_chunk;

    /* Track the stream sequence numbers.  Require in-order, no gaps. */
    uintptr_t expected_chunk_nb;

    uint32_t valid;

};

static inline void
fwstream_reset(struct fwstream *s) {
    /* It is assumed that user has already filled in the callbacks,
       chunk_size, max_size and priority.  For the rest we only need
       to set the expected first block.  The fwstream_push() routine
       will init everything from the first block. */
    s->expected_chunk_nb = 0;
}

static inline uint32_t
fwstream_size_padded(uint32_t size_bytes) {  // endx-start
    return (((size_bytes-1)/BLOCK_SIZE)+1)*BLOCK_SIZE;
}
static inline uint32_t
fwstream_ctrl_crc(struct fwstream *s, const struct gdbstub_control *c) {
    return s->checksum_inc((void*)c, c->size - sizeof(uint32_t), 0);
}
static inline uint32_t
fwstream_new_priority(struct fwstream *s, const struct gdbstub_config *config) {
    uint32_t size_padded =
        fwstream_size_padded(config->flash_endx - config->flash_start);
    struct gdbstub_control *c = (void*)(config->flash_start + size_padded);

    uint32_t priority = 0;
    if (c &&
        (c->size > BLOCK_SIZE) /* sanity check */) {

        uint32_t crc = fwstream_ctrl_crc(s, c);
        if (c->ctrl_crc == crc) {
            priority = c->priority + 1;
        }
        else {
            LOG("fwstream_ctrl_crc: %x != %x\n", c->ctrl_crc, crc);
        }
    }
    else {
        /* Test path: config can have _start and _endx set to zero to
           indicate there is no target Flash. */
        LOG("fwstream_ctrl_crc: no gdbstub_control\n");
    }
    return priority;

}


static inline int
fwstream_push(struct fwstream *s, uintptr_t chunk_nb, const uint8_t *chunk_data) {

    /* There are two cases to handle:

       - First block contains the metadata block of the first
         partition

       - Subsequent blocks contain payload data, trailer data or
         header data of subsequent partitions.

       The format is fairly ad-hoc and grew over time.
       Coincidentally, it is ordered in such a way that it can be
       concetenated and streamed.

    */
    struct gdbstub_control new_c;

    /* Start ignoring once the sequence is broken. */
    if (chunk_nb != s->expected_chunk_nb) {
        LOG("chunk_nb = %d, expected = %d\n", chunk_nb, s->expected_chunk_nb);
        return FWSTREAM_ERR_GAP;
    }
    s->expected_chunk_nb = chunk_nb + 1;

    /* Parse gdbstub_config header. */
    if (0 == chunk_nb) {
        /* This is semi platform-independent.  Don't rely on pointer
           size, and use a flat uint32_t array for the struct.  Assume
           that chunk size is large enough so it contains a complete
           header.  Assume that chunks are aligned properly such that
           the beginning of the chunk aligns with the beginning of a
           header.  Assume host endianness (e.g. 64 bit intel) is the
           same as target (e.g. 32 bit arm).. */
        const uint32_t *fw = (const void*)chunk_data;
        uint32_t start = fw[GDBSTUB_CONFIG_INDEX_FLASH_START];
        uint32_t endx  = fw[GDBSTUB_CONFIG_INDEX_FLASH_ENDX];
        s->start_addr = start;
        s->endx_addr = endx;
        s->checksum_acc = 0;
        s->valid = 0;

        /* Perform some consistency checks.  If any of these fail,
           we've lost track and need to abort the iteration. */
        if (endx <= start) return FWSTREAM_ERR_FW_ENDX;
        uint32_t size_bytes = endx - start;
        uint32_t size_padded = fwstream_size_padded(size_bytes);
        if (s->max_size && (size_padded > s->max_size)) {
            LOG("fwstream: max_size=0x%x, size_padded=0x%x\n",
                s->max_size, size_padded);
            return FWSTREAM_ERR_FW_SIZE;
        }
        s->control_chunk = size_padded / s->chunk_size;

        LOG("fwstream: partition: %x %x %d (0x%x)\n",
            start, endx, size_bytes, size_bytes);

    }

    /* Parse gdbstub_control header. */
    if (s->control_chunk == chunk_nb) {
        /* The control struct doesn't contain pointers so we can just
           use it.  Still assuming same endianness. */
        const struct gdbstub_control *c = (const void*)chunk_data;
        if ((c->size != sizeof(struct gdbstub_control))) return FWSTREAM_ERR_CTRL_SIZE;
        uint32_t ctrl_crc = fwstream_ctrl_crc(s, c);

        LOG("crc ctrl: %08x (%08x)\n", ctrl_crc, c->ctrl_crc);
        if (ctrl_crc != c->ctrl_crc) return FWSTREAM_ERR_CTRL_CRC;

        /* Validate the control header, assuming it fits in one chunk,
           then validate the incremental checksum. */
        LOG("crc fw:   %08x (%08x)\n", s->checksum_acc, c->fw_crc);
        if (s->checksum_acc != c->fw_crc) return FWSTREAM_ERR_FW_CRC;

        /* Optionally update the priority. */
        if (c->priority != s->priority) {
            LOG("priority: %d -> %d\n", c->priority, s->priority);
            /* Chunk is const, so replace control block chunk with copy. */
            new_c = *c;
            chunk_data = (const uint8_t*)&new_c;
            new_c.priority = s->priority;
            new_c.ctrl_crc = fwstream_ctrl_crc(s, &new_c);
            LOG("crc ctrl: %08x (updated)\n", new_c.ctrl_crc);
        }

        s->valid = 1;
        LOG("fw receive OK\n");
    }

    /* Write chunk. */
    const uint8_t *written = s->write(s, chunk_nb, chunk_data);

    /* Compute checksum incrementally from the memory that was
       written.  (In practice it takes too long to compute it in a
       single shot.) */
    if (written) {
        uint32_t start = s->start_addr + s->chunk_size * chunk_nb;
        if (start < s->endx_addr) { // Only firmware chunks
            uint32_t endx  = start + s->chunk_size;
            // FW is not padded to chunk size, so adjust span of the last block.
            if (endx > s->endx_addr) { endx = s->endx_addr; }
            s->checksum_acc =
                s->checksum_inc(written, endx - start, s->checksum_acc);
        }
        return 0;
    }
    else {
        /* This is not a great error message.  Likely what happened
           here is that we're trying to overwrite the running
           partition, and s->write() won't allow that.  It's probably
           best to add a check earlier on so we won't end up here. */
        return FWSTREAM_ERR_NOT_WRITTEN;
    }
}

#endif
