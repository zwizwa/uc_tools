/* Tool to create the gdbstub_control block that is appended to a
   binary firmware image.  The other side of this is trampoline.c
   which is a gdbstub application acting as a chain loader for
   firmware images in the .fw format.

   This assumes that host and target have same endianness.  For now it
   seems that there is not going to be an exception to this, but
   beware!
*/

#include "crc.h"
#include "macros.h"
#include "gdb/gdbstub_api.h"
#include <stdio.h>

/* bin2fw <bin> [<fw>] */
int main(int argc, char **argv) {

    /* Read bin file. */
    ASSERT(argc == 3);
    FILE *f, *f_fw;
    LOG("opening %s\n", argv[1]);
    ASSERT(f = fopen(argv[1], "r"));
    ASSERT(0 == fseek(f, 0, SEEK_END));
    uint32_t len = ftell(f);

    /* Pad to block size. */
    uint32_t block_logsize = 10;
    uint32_t block_size = 1 << block_logsize;
    uint32_t len_padded = (((len-1)>>block_logsize)+1)<<block_logsize;
    LOG("len    = 0x%08x (%d)\n", len, len);
    LOG("padded = 0x%08x (%d)\n", len_padded, len_padded);

    /* The fw output image will have one extra block. */
    uint32_t fw_len = len_padded + block_size;

    /* Note that we can't use the struct directly, as pointers are 32
       bit, so treat it as a uint32_t array. */
    uint32_t fw[fw_len / 4];
    memset(fw, 0, sizeof(fw));
    ASSERT(0 == fseek(f, 0, SEEK_SET));
    ASSERT(len == fread(fw, 1, len, f));

    /* Buffer now contains 0-padded firmware.  Verify header.  Note
       that we can't use the struct directly, as pointers are 32 bit,
       so treat it as a uint32_t array. */
    uint32_t start = fw[GDBSTUB_CONFIG_INDEX_FLASH_START];
    uint32_t endx  = fw[GDBSTUB_CONFIG_INDEX_FLASH_ENDX];
    LOG("start  = 0x%08x\n", start);
    LOG("endx   = 0x%08x\n", endx);

    /* Some consistency checks.  FIXME: Hardcoded for 128k STM32F103.
       This should also catch endianness errors. */
    ASSERT(start == 0x08004000 || start == 0x08012000);
    ASSERT(endx  == start + len_padded);

    /* Fill the control block. */
    uint32_t crc = crc32b((uint8_t*)fw, len_padded);
    LOG("crc    = 0x%08x\n", crc);
    uint32_t *control = fw + len_padded/4;

    control[GDBSTUB_CONTROL_INDEX_VERSION] = 0; // not yet uesd
    control[GDBSTUB_CONTROL_INDEX_SIZE]    = block_size;
    control[GDBSTUB_CONTROL_INDEX_CRC]     = crc;


    /* Write out the .fw image = padded .bin + control block appended. */
    ASSERT(f_fw = fopen(argv[2], "w"));
    ASSERT(fw_len = fwrite(fw, 1, fw_len, f_fw));
    fclose(f_fw);

    return 0;
}
