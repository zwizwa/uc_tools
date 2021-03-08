#include "sha1.h"
#include "sha256.h"
#include "macros.h"
#include <stdint.h>

void test_sha1(void) {
    SHA1_CTX ctx;
    sha1_init(&ctx);
    const char data[] = "test123";
    sha1_update(&ctx, (BYTE*)data, strlen(data));
    uint8_t hash[SHA1_BLOCK_SIZE] = {};
    sha1_final(&ctx, hash);
    for(int i=0; i<sizeof(hash); i++) LOG("%02x", hash[i]);
    LOG("\n");

    // computed by https://www.functions-online.com/sha1.html
    // 7288edd0fc3ffcbe93a0cf06e3568e28521687bc

    // produced by this file
    // 7288edd0fc3ffcbe93a0cf06e3568e28521687bc
}

void test_sha256(void) {
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

int main(void) {
    LOG("%s\n", __FILE__);
    test_sha1();
    test_sha256();

}
