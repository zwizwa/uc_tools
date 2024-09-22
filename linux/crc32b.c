#include "crc.h"
#include "macros.h"
#include "stm32f103/gdbstub_api.h"

#include <stdio.h>

int main(int argc, char **argv) {
    ASSERT(argc == 2 || argc == 3);
    FILE *f;
    LOG("opening %s\n", argv[1]);
    ASSERT(f = fopen(argv[1], "r"));
    ASSERT(0 == fseek(f, 0, SEEK_END));
    long len = ftell(f);
    LOG("len = %d (0x%x)\n", len, len);
    uint32_t offset =
        (argc == 3) ?
        strtol(argv[2], NULL, 0) : 0;
    LOG("offset = %d (0x%x)\n", offset, offset);
    ASSERT(offset <= len);
    ASSERT(0 == fseek(f, offset, SEEK_SET));
    len -= offset;
    uint8_t buf[len];
    ASSERT(len == fread(buf, 1, len, f));
    uint32_t crc = crc32b(buf, len);
    LOG("crc = 0x%08x\n", crc);
    return 0;
}
