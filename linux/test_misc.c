#include "macros.h"
#include "byteswap.h"


int main(int argc, char **argv) {
    LOG("test_misc.c\n");
    ASSERT(0x1234 == SWAP_U16(0x3412));
    ASSERT(0x12345678 == SWAP_U32(0x78563412));
}
