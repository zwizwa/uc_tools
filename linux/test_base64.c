#include <stdio.h>
#include <string.h>
#include "base64.h"

#include "macros.h"

static int tests_run = 0;
static int tests_failed = 0;

#define CHECK(cond, ...)                                   \
    do {                                                   \
        tests_run++;                                       \
        if (!(cond)) {                                     \
            tests_failed++;                                \
            LOG("FAIL %s:%d: ", __FILE__, __LINE__);       \
            LOG(__VA_ARGS__);                              \
            LOG("\n");                                     \
        }                                                  \
    } while (0)

/* RFC 4648 test vectors */
static const struct {
    const char *plain;
    const char *b64;
} vectors[] = {
    {"", ""},
    {"f", "Zg=="},
    {"fo", "Zm8="},
    {"foo", "Zm9v"},
    {"foob", "Zm9vYg=="},
    {"fooba", "Zm9vYmE="},
    {"foobar", "Zm9vYmFy"},
};

static void test_encode(void) {
    for (size_t v = 0; v < sizeof(vectors) / sizeof(vectors[0]); v++) {
        size_t in_len = strlen(vectors[v].plain);
        char out[64] = {0};
        base64_encode(out, (const unsigned char *)vectors[v].plain, in_len);
        out[base64_length(in_len)] = '\0'; /* encode does not null-terminate */
        CHECK(strcmp(out, vectors[v].b64) == 0,
              "encode(\"%s\") = \"%s\", expected \"%s\"",
              vectors[v].plain, out, vectors[v].b64);
    }
    CHECK(base64_length(0) == 0, "base64_length(0)");
    CHECK(base64_length(1) == 4, "base64_length(1)");
    CHECK(base64_length(3) == 4, "base64_length(3)");
    CHECK(base64_length(4) == 8, "base64_length(4)");
}

static void test_decode(void) {
    for (size_t v = 0; v < sizeof(vectors) / sizeof(vectors[0]); v++) {
        unsigned char out[64] = {0};
        size_t out_len = 0;
        int rc = base64_decode(out, vectors[v].b64, strlen(vectors[v].b64), &out_len);
        CHECK(rc == 0, "decode(\"%s\") rc=%d", vectors[v].b64, rc);
        CHECK(out_len == strlen(vectors[v].plain),
              "decode(\"%s\") len=%zu, expected %zu",
              vectors[v].b64, out_len, strlen(vectors[v].plain));
        CHECK(memcmp(out, vectors[v].plain, out_len) == 0,
              "decode(\"%s\") content mismatch", vectors[v].b64);
    }
}

static void test_decode_invalid(void) {
    unsigned char out[64];
    size_t n;
    CHECK(base64_decode(out, "Zg=", 3, &n) == -1, "length not multiple of 4");
    CHECK(base64_decode(out, "Zm9v!A==", 8, &n) == -1, "invalid character");
    CHECK(base64_decode(out, "Zg==Zm9v", 8, &n) == -1, "padding not at end");
    CHECK(base64_decode(out, "Z===", 4, &n) == -1, "three padding chars");
    CHECK(base64_decode(out, "=m9v", 4, &n) == -1, "padding at start");
}

static void test_roundtrip_binary(void) {
    unsigned char data[256];
    for (int i = 0; i < 256; i++) data[i] = (unsigned char)i;

    char enc[base64_length(256) + 1];
    base64_encode(enc, data, sizeof(data));
    enc[base64_length(256)] = '\0';

    unsigned char dec[256];
    size_t dec_len = 0;
    int rc = base64_decode(dec, enc, strlen(enc), &dec_len);
    CHECK(rc == 0, "binary roundtrip rc=%d", rc);
    CHECK(dec_len == sizeof(data), "binary roundtrip len=%zu", dec_len);
    CHECK(memcmp(dec, data, sizeof(data)) == 0, "binary roundtrip content");
}

int main(void) {
    test_encode();
    test_decode();
    test_decode_invalid();
    test_roundtrip_binary();

    LOG("%d/%d tests passed\n", tests_run - tests_failed, tests_run);
    return tests_failed ? 1 : 0;
}

