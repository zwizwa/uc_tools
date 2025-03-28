/* Tool to create the gdbstub_control block that is appended to a
   binary firmware image.  The other side of this is trampoline.c
   which is a gdbstub application acting as a chain loader for
   firmware images in the .fw format.

   This assumes that host and target have same endianness.  For now it
   seems that there is not going to be an exception to this, but
   beware!
*/

#include "sha1.h"
#include "crc.h"
#include "macros.h"
#include "stm32f103/gdbstub_api.h"
#include <stdio.h>

int bin2fw(
    const char *bin,      // input: binary image
    const char *fw,       // output: binary firmware image
    const char *fwctrl,   // output: firmware control block only
    const char *sha1      // output: firmware sha1
    ){

    /* Read bin file. */
    LOG("=== BIN2FW <- %s\n", bin);
    FILE *f, *f_fw;
    ASSERT(NULL != (f = fopen(bin, "r")));
    ASSERT(0 == fseek(f, 0, SEEK_END));
    uint32_t bin_len = ftell(f);

    /* Require that the file is already padded and that it contains a
       dummy control block.  Use the pointer to the control block and
       the size of the file to obtain the block size, only for assert
       that it is either 1k or 2k.  The block size is not needed for
       anything else. */

    LOG("bin_len = 0x%08x (%d) input file length\n", bin_len, bin_len);

    /* The input file possibly already has a (dummy) control block.
       We can't tell until after reading the file.  To determine the
       size of the buffer, add one block to cover the case where the
       control block is not included.  We model the firmware as
       uint32_t so we can easily read the fields.  Note that we do
       assume endianness is the same between host and
       target. (FIXME!) */
    uint32_t fw_u32[bin_len / sizeof(uint32_t)];
    uint8_t *fw_u8 = (void*)fw_u32;
    memset(fw_u32, 0, sizeof(fw_u32));
    ASSERT(0 == fseek(f, 0, SEEK_SET));
    ASSERT(bin_len == fread(fw_u32, 1, bin_len, f));

    /* Buffer now contains 0-padded firmware.  Verify header.  Note
       that we can't use the struct directly, as pointers are 32 bit,
       so treat it as a uint32_t array. */
    uint32_t start = fw_u32[GDBSTUB_CONFIG_INDEX_FLASH_START];
    uint32_t endx  = fw_u32[GDBSTUB_CONFIG_INDEX_FLASH_ENDX];
    LOG("start   = 0x%08x\n", start);
    LOG("endx    = 0x%08x\n", endx);

    /* Some consistency checks.  FIXME: Hardcoded for 128k or 256k
       STM32F103.  This should also catch endianness errors. */
    ASSERT(start == 0x08002800 || /* Main application partition. */
           start == 0x08004000 || /* trampoline.c partition a... */
           start == 0x08012000 || /* ... and b */
           start == 0x08022000    /* partition b for 256k devices */
           );

    ASSERT(endx > start);
    uint32_t span = endx - start;
    LOG("span    = 0x%08x (%d) span of original firmware\n", span, span);

    /* Find control block location, calcluate block size */
    uint32_t control_addr = fw_u32[GDBSTUB_CONFIG_INDEX_CONTROL];
    LOG("control = 0x%08x\n", control_addr);
    ASSERT(control_addr > start);
    uint32_t control_offset = control_addr - start;
    ASSERT(control_offset < bin_len);
    uint32_t bsize = bin_len - control_offset;
    LOG("bsize   = 0x%08x (%d) flash block size\n", bsize, bsize);
    ASSERT(bsize == 1024 ||
           bsize == 2048);
    ASSERT((bin_len % bsize) == 0);

    /* Looks ok.  Fill the control block. */
    uint32_t fw_crc = crc32b(fw_u8, span);
    LOG("crc     = 0x%08x checksum of original firmware\n", fw_crc);
    struct gdbstub_control *control = (void*)(fw_u8 + control_offset);

    /* Calculate SHA1 of the config header + firmware.  Note that the
       semantics changed here.  We are hashing only the firmware
       binary image, not the ELF file.  This allows firmware to
       self-validate if needed, and if we use the hash in a CAS
       database also still allows mapping from binary firmware to ELF
       to load debug symbols from ELF CAS database.  We do give up the
       direct link from ELF to hash, which is a small price to pay for
       the extra functionality. */
    SHA1_CTX ctx;
    sha1_init(&ctx);
    sha1_update(&ctx, fw_u8, span);
    sha1_final(&ctx, &control->fw_sha1[0]);
    char sha1_hex[20*2+2];
    LOG("sha1    = ");
    for(int i=0; i<20; i++) { sprintf(sha1_hex + 2*i, "%02x", control->fw_sha1[i]); }
    sprintf(sha1_hex + 40, "\n");
    LOG("%s", sha1_hex);

    /* Fill in the rest of the control block. */
    control->version = 0;
    control->fw_crc = fw_crc;
    control->size = sizeof(*control);
    control->ctrl_crc = crc32b((uint8_t*)control, control->size - 4);

    /* Write out the .fw image = padded .bin + control block appended. */
    LOG("firmware image: %s\n", fw);
    ASSERT(NULL != (f_fw = fopen(fw, "w")));
    ASSERT(bin_len == fwrite(fw_u8, 1, bin_len, f_fw));
    fclose(f_fw);

    /* Write out the control block to a separate file. */
    LOG("control block:  %s\n", fwctrl);
    FILE *f_fwctrl;
    ASSERT(NULL != (f_fwctrl = fopen(fwctrl, "w")));
    ASSERT(bsize == fwrite((void*)control, 1, bsize, f_fwctrl));
    fclose(f_fwctrl);

    /* Write the sha1 hex to a separate file. */
    LOG("sha1 hex:       %s\n", sha1);
    FILE *f_sha1;
    ASSERT(NULL != (f_sha1 = fopen(sha1, "w")));
    ASSERT(41 == fwrite(sha1_hex, 1, 41, f_sha1));
    fclose(f_sha1);

    LOG("=== BIN2FW\n"
        "-> %s\n"
        "-> %s\n"
        "-> %s\n",
        fw, fwctrl, sha1);

    return 0;
}

/* bin2fw <bin> <fw> <fwctrl> <sha1_hex> */
int main(int argc, char **argv) {
    ASSERT(argc == 5);
    return bin2fw(argv[1],argv[2],argv[3],argv[4]);
}

