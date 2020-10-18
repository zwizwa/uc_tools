#ifndef FWSTREAM_H
#define FWSTREAM_H

#include <stdint.h>
#include "macros.h"
#include "gdbstub_api.h"

/* Decoder for concatenated firmware image stream.

   This contains the platfor-independent part.  See
   linux/test_fwstream.c

   This is useful for streaming a firmware image partition set on a
   broadcast bus, where the image contains multiple partitions, and
   each device determines for itself which one to select.  The format
   uses the gdbstub_api.h header and a trailing checksum header.  See
   also linux/bin2fw.c */

/* Firmware blocksize is hardcoded to STM32F103 erase block size. */
#define BLOCK_LOGSIZE 10
#define BLOCK_SIZE    (1 << BLOCK_LOGSIZE)

struct fwstream {
    void (*write)(uintptr_t addr, const uint8_t *chunk, uint32_t len);
    uintptr_t chunk_size; // It's simpler to keep this constant.
    uintptr_t fw_offset;
    uintptr_t fw_endx_chunk;
};

static inline void
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

    /* Automatic stream reset. */
    if (0 == chunk_nb) {
        s->fw_endx_chunk = 0;
    }

    /* Header chunk. */
    if (s->fw_endx_chunk == chunk_nb) {
        /* This is platform-independent, so we cannot rely on struct
         * gdbstub_config.  Use a flat array.  Also, assume that chunk
         * size is large enough so it contains a complete header, and
         * that chunks are aligned properly such that the beginning of
         * the chunk aligns with the beginning of a header. */
        const uint32_t *fw = (const void*)chunk_data;
        uint32_t start = fw[GDBSTUB_CONFIG_INDEX_FLASH_START];
        uint32_t endx  = fw[GDBSTUB_CONFIG_INDEX_FLASH_ENDX];
        /* FIXME: check consistency of pointers! */

        uint32_t size = endx - start;
        LOG("flash: %x %x %d (0x%x)\n", start, endx, size, size);
        /* From this the location of the next partition header can be found. */
        s->fw_endx_chunk += (size + BLOCK_SIZE) / s->chunk_size;
        s->fw_offset = start;
    }

    /* Note that in practice, data needs to be a power of two, to make
       writing to Flash easier to implement. */
    uintptr_t addr = s->fw_offset + s->chunk_size * chunk_nb;

    if (s->write) {
        s->write(addr, chunk_data, s->chunk_size);
    }
    else {
        //LOG("write: %x %d\n", addr, s->chunk_size);
    }
}

#endif
