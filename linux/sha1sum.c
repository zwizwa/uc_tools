#include "sha1.h"
#include "macros.h"
#include <stdio.h>

int bin2fw(
    const char *bin  // input: binary image
    ){
    /* Read bin file. */
    FILE *f;
    ASSERT(NULL != (f = fopen(bin, "r")));
    ASSERT(0 == fseek(f, 0, SEEK_END));
    long bin_len = ftell(f);
    uint8_t buf[bin_len];
    ASSERT(0 == fseek(f, 0, SEEK_SET));
    ASSERT(bin_len == fread(buf, 1, bin_len, f));

    SHA1_CTX ctx;
    sha1_init(&ctx);
    sha1_update(&ctx, (const uint8_t*)buf, bin_len);
    uint8_t hash[20];
    sha1_final(&ctx, hash);
    for(int i=0; i<20; i++) { printf("%02x", hash[i]); }  printf("\n");
    return 0;
}

int main(int argc, char **argv) {
    if (argc != 2) {
        LOG("usage: %s <infile>\n", argv[0]);
        return 1;
    }
    return bin2fw(argv[1]);
}

