#include "macros.h"
#include "crc.h"
#include "gdb/fwstream.h"

uint8_t chunk[128];
const uint8_t *fw_write(struct fwstream *fw, uintptr_t chunk_nb, const uint8_t *data) {
    memcpy(chunk, data, fw->chunk_size);
    return &chunk[0];
}


int main(int argc, char **argv) {
    LOG("%s\n", argv[0]);

    /* Read bin file. */
    ASSERT(argc == 2);
    FILE *f_fw;
    LOG("opening %s\n", argv[1]);
    ASSERT(f_fw = fopen(argv[1], "r"));
    ASSERT(0 == fseek(f_fw, 0, SEEK_END));
    uint32_t fw_len = ftell(f_fw);
    LOG("fw_len = %d (0x%x)\n", fw_len, fw_len);

    uint8_t fw[fw_len];
    memset(fw, 0, sizeof(fw));
    ASSERT(0 == fseek(f_fw, 0, SEEK_SET));
    ASSERT(fw_len == fread(fw, 1, fw_len, f_fw));

    struct fwstream fws = {
        .chunk_size = sizeof(chunk),
        .max_size = 0xD000,
        .write = fw_write,
        .checksum_inc = crc32b_inc
    };
    fwstream_reset(&fws);
    for (int i=0; i<fw_len/fws.chunk_size; i++) {
        ASSERT(0 == fwstream_push(&fws, i, fw + i*fws.chunk_size));
    }

    return 0;
}
