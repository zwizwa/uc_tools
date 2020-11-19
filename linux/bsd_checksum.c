#include <stdio.h>
#include "crc.h"
int main(int argc, char **argv) {
    uint16_t state = 0;
    int c;
    while(EOF != (c = getchar())) {
        uint8_t b = c;
        state = bsd_checksum_inc(&b, 1, state);
    }
    printf("0x%04x\n", state);
    return 0;
}
