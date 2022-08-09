#include "macros.h"
#include "../blowfish/blowfish.h"
#include "../blowfish/blowfish.c"
#include "../blowfish/blowfish_dat.c"

int main(int argc, char **argv) {

    puts("Blowfish testing function...");
    blowfish_context_t *ctx = (blowfish_context_t *)malloc(sizeof(blowfish_context_t));
    if(!ctx) {
        puts("Could not allocate enough memory!");
        return -1;
    }
    blowfish_initiate(ctx, "TESTKEY", 7);
    uint32_t hi = 1, lo = 2;
    blowfish_encryptblock(ctx, &hi, &lo);
    printf("Encoded: %08lX %08lX\n", hi, lo);
    if((hi == 0xDF333FD2) && (lo == 0x30A71BB4))
        puts("Encryption Test Passed");
    else puts("Encryption Test Failed");
    blowfish_decryptblock(ctx, &hi, &lo);
    if((hi == 1) && (lo == 2))
        puts("Decryption Test Passed");
    else puts("Decryption Test Failed");

    puts("Done!");
    blowfish_clean(ctx);
    free(ctx);
    return 0;

}
