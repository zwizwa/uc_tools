#ifndef MOD_AUTH
#define MOD_AUTH

// Better-than-nothing SHA256 MAC with time stamp checking.

#include "macros.h"
#include "uct_byteswap.h"
#include <stdint.h>

/* AUTHENTICATION */
uint8_t auth_key[32];
static inline time_t auth_now(void) { return time(NULL); }

void load_auth_key(const char *auth_key_file) {
    FILE *f;
    ASSERT(f = fopen(auth_key_file, "r"));
    ASSERT(sizeof(auth_key) == fread(auth_key, 1, sizeof(auth_key), f));
    fclose(f);
}
struct auth {
    uint8_t time[sizeof(time_t)];
    uint8_t mac[32];
} __attribute__((__packed__));
#include "sha256.h"
#include "sha256.c"
void sha256(const uint8_t *buf, size_t len, uint8_t *sum) {
    SHA256_CTX ctx;
    sha256_init(&ctx);
    sha256_update(&ctx, buf, len);
    sha256_final(&ctx, sum);
}
void auth_stamp(uint8_t *buf, size_t total_len) {
    ASSERT(8 == sizeof(time_t));
    ASSERT(total_len >= sizeof(struct auth));
    size_t len = total_len - sizeof(struct auth);
    struct auth *auth = (void*)(buf + len);
    write_be(auth->time, auth_now(), sizeof(auth->time));
    size_t stamped_len = len + sizeof(auth->time);
    sha256(buf, stamped_len, auth->mac);
}
int auth_check(uint8_t *buf, size_t total_len) {
    ASSERT(total_len >= sizeof(struct auth));
    size_t len = total_len - sizeof(struct auth);
    struct auth *auth = (void*)(buf + len);
    size_t stamped_len = len + sizeof(auth->time);
    uint8_t mac[sizeof(auth->mac)];
    sha256(buf, stamped_len, mac);
    int mac_ok = !memcmp(mac, auth->mac, sizeof(auth->mac));

    int64_t t_them = read_be(auth->time, 8);
    int64_t t_us   = auth_now();

    int64_t t_diff = t_us - t_them;
    if ((t_diff > 3) || (t_diff < -3)) {
        LOG("bad t_diff=%ld\n", t_diff);
        return 0;
    }
    else {
        // LOG("t_them=%ld, t_diff=%ld\n", t_them, t_diff);
    }

    // FIXME: check timestamp
    return mac_ok;
}



#endif
