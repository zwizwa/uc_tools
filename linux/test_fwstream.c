#include "macros.h"
#include "gdb/fwstream.h"

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

    struct fwstream fws = { .chunk_size = 128 };
    for (int i=0; i<fw_len/fws.chunk_size; i++) {
        fwstream_push(&fws, i, fw + i*fws.chunk_size);
    }

    return 0;
}
