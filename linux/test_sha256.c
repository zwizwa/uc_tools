#include "sha256.h"
#include "macros.h"
#include <stdint.h>



int main(void) {
    LOG("%s\n", __FILE__);
    SHA256_CTX ctx;
    sha256_init(&ctx);
    const char data[] = "test123";
    sha256_update(&ctx, (BYTE*)data, strlen(data));
    uint8_t hash[SHA256_BLOCK_SIZE] = {};
    sha256_final(&ctx, hash);
    for(int i=0; i<sizeof(hash); i++) LOG("%02x", hash[i]);
    LOG("\n");

    // computed by https://www.freeformatter.com/sha256-generator.html#ad-output
    // ecd71870d1963316a97e3ac3408c9835ad8cf0f3c1bc703527c30265534f75ae

    // produced by this file
    // ecd71870d1963316a97e3ac3408c9835ad8cf0f3c1bc703527c30265534f75ae

}
