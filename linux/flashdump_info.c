/* Read 128k Flash dump from STM32F103xB (or x8 with 128kByte) and
   prints out some information.

   Adapted from bin2fw.c - maybe time to make a library?

*/

#include "crc.h"
#include "macros.h"
#include "stm32f103/gdbstub_api.h"
#include <stdio.h>

uint8_t flash[0x20000];

void assert_flash(uint32_t addr) {
    ASSERT(addr >= 0x08000000);
    ASSERT(addr < (0x08000000 + sizeof(flash)));
}

void partition_info(uint32_t offset, const char *format) {
    LOG("partition at offset 0x%08x\n", offset);
    uint8_t *fw_u8 = flash + offset;
    uint32_t *fw_u32 = (uint32_t*)fw_u8;
    uint32_t start = fw_u32[GDBSTUB_CONFIG_INDEX_FLASH_START];
    uint32_t endx  = fw_u32[GDBSTUB_CONFIG_INDEX_FLASH_ENDX];
    LOG("start    0x%08x\n", start); assert_flash(start);
    LOG("endx     0x%08x\n", endx);  assert_flash(start);

    uint32_t span = endx - start;

    uint32_t block_logsize = 10;
    uint32_t span_pad = (((span-1)>>block_logsize)+1)<<block_logsize;
    ASSERT(span_pad <= sizeof(flash) + sizeof(struct gdbstub_control));

    struct gdbstub_control *control = (void*)(fw_u8 + span_pad);
    ASSERT(control->size <= (1 << block_logsize));
    uint32_t ctrl_crc = crc32b((uint8_t*)control, control->size - 4);
    LOG("ctrl_crc 0x%08x\n", ctrl_crc);
    ASSERT(ctrl_crc == control->ctrl_crc);
    LOG("ctrl_crc OK\n");

    uint32_t fw_crc = crc32b(fw_u8, span);
    LOG("fw_crc   0x%08x\n", fw_crc);
    ASSERT(fw_crc == control->fw_crc);
    LOG("fw_crc   OK\n");

    uint32_t version_addr = fw_u32[GDBSTUB_CONFIG_INDEX_VERSION];
    uint8_t *version = &flash[version_addr - 0x08000000];
    LOG("version  %s\n", version);

    char sha1[20*2+1] = {};
    for (uint32_t i=0; i<sizeof(control->fw_sha1); i++) {
        sprintf(sha1 + 2*i, "%02x", control->fw_sha1[i]);
    }
    LOG("elf_sha1 %s\n", sha1);

    if (format && (!strcmp(format, "shell_vars"))) {
        /* Print variables to stdout */
        printf("version=\"%s\"\n", version);
        printf("elf_sha1=\"%s\"\n", sha1);
    }
    else {
        ERROR("unknown output format '%s'\n", format);
    }

}

int flashdump_info(
    const char *bin,        // input: binary image
    const char *offset_str, // input: offset of partition header
    const char *format      // input: output format
    ) {

    /* For implementation convenience it is assumed that host is
       little endian, same as the Cortex M target. */
    uint8_t endian_u8[4] = {1,2,3,4};
    ASSERT(*((uint32_t*)endian_u8) == 0x04030201);

    /* Read bin file. */
    LOG("=== FLASHDUMP_INFO <- %s\n", bin);
    FILE *f;
    ASSERT(NULL != (f = fopen(bin, "r")));
    ASSERT(0 == fseek(f, 0, SEEK_END));
    ASSERT(sizeof(flash) == ftell(f));
    ASSERT(0 == fseek(f, 0, SEEK_SET));
    ASSERT(sizeof(flash) == fread(flash, 1, sizeof(flash), f));

    LOG("%d bytes read\n", sizeof(flash));

    uint32_t offset = strtol(offset_str, NULL, 0);

    partition_info(offset, format);
    exit(0);

    return 0;
}

/* bin2fw <bin> <offset> */
int main(int argc, char **argv) {
    ASSERT(argc == 4);
    return flashdump_info(argv[1], argv[2], argv[3]);
}

