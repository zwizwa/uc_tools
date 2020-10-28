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

/* bin2fw <bin> <fw> [<fwctrl>] */
int main(int argc, char **argv) {

    /* Read bin file. */
    ASSERT(argc == 3 || argc == 4);
    FILE *f, *f_fw;
    LOG("%s: opening %s\n", argv[0], argv[1]);
    ASSERT(f = fopen(argv[1], "r"));
    ASSERT(0 == fseek(f, 0, SEEK_END));
    uint32_t bin_len = ftell(f);

    /* Pad to block size. */
    uint32_t block_logsize = 10;
    uint32_t block_size = 1 << block_logsize;
    uint32_t bin_pad = (((bin_len-1)>>block_logsize)+1)<<block_logsize;
    LOG("bin_len = 0x%08x (%d) input file length\n", bin_len, bin_len);
    LOG("bin_pad = 0x%08x (%d) ... padded to block size\n", bin_pad, bin_pad);

    /* The input file possibly already has a (dummy) control block.
       We can't tell until after reading the file.  To determine the
       size of the buffer, add one block to cover the case where the
       control block is not included.  We model the firmware as
       uint32_t so we can easily read the fields.  Note that we do
       assume endianness is the same between host and
       target. (FIXME!) */
    uint32_t fw[(bin_pad + block_size) / sizeof(uint32_t)];
    memset(fw, 0, sizeof(fw));
    ASSERT(0 == fseek(f, 0, SEEK_SET));
    ASSERT(bin_len == fread(fw, 1, bin_len, f));

    /* Buffer now contains 0-padded firmware.  Verify header.  Note
       that we can't use the struct directly, as pointers are 32 bit,
       so treat it as a uint32_t array. */
    uint32_t start = fw[GDBSTUB_CONFIG_INDEX_FLASH_START];
    uint32_t endx  = fw[GDBSTUB_CONFIG_INDEX_FLASH_ENDX];
    LOG("start   = 0x%08x\n", start);
    LOG("endx    = 0x%08x\n", endx);

    /* Some consistency checks.  FIXME: Hardcoded for 128k STM32F103.
       This should also catch endianness errors. */
    ASSERT(start == 0x08002800 || /* Main application partition. */
           start == 0x08004000 || /* trampoline.c partition a... */
           start == 0x08012000);  /* ... and b */

    ASSERT(endx > start);
    uint32_t span = endx - start;
    LOG("span    = 0x%08x (%d) block-padded span of original firmware\n", span, span);

    /* Make sure the span is padded to block_size.  This is done by
       the linker script, see ALIGN(1024) before definition of
       _eflash. */
    ASSERT((span & (block_size - 1)) == 0);

    /* Looks ok.  Add the control block.  We don't really care if the
       original file had a control block as we will just overwrite
       it. */
    uint32_t fw_len = span + block_size;
    LOG("fw_len  = 0x%08x (%d) firmware size with control block\n", fw_len, fw_len);

    /* Make sure it fits in the buffer. */
    ASSERT(fw_len <= sizeof(fw));

    /* Fill the control block. */
    uint32_t fw_crc = crc32b((uint8_t*)fw, span);
    LOG("crc     = 0x%08x checksum of original firmware\n", fw_crc);
    struct gdbstub_control *control = (void*)(fw + span/4);

    control->version = 0;
    control->fw_crc = fw_crc;
    control->size = sizeof(*control);
    control->ctrl_crc = crc32b((uint8_t*)control, control->size - 4);

    /* Write out the .fw image = padded .bin + control block appended. */
    LOG("firmware image: %s\n", argv[2]);
    ASSERT(f_fw = fopen(argv[2], "w"));
    ASSERT(fw_len = fwrite(fw, 1, fw_len, f_fw));
    fclose(f_fw);

    /* Optionally, write out only the control block. */
    if (argc == 4) {
        LOG("control block:  %s\n", argv[3]);
        FILE *f_fwctrl;
        ASSERT(f_fwctrl = fopen(argv[3], "w"));
        ASSERT(block_size = fwrite((void*)control, 1, block_size, f_fwctrl));
        fclose(f_fwctrl);
    }

    return 0;
}
