#include <stdint.h>
#define NS(suffix) rle_enc##suffix 
typedef uint8_t NS(_t);
#include "ns_rle_enc.h"
#include "assert_write.h"

void NS(_write)(struct NS(_state) *s) {
    assert_write(1, (uint8_t*)s->out, s->out_i * sizeof(NS(_t)));
}
ssize_t NS(_read)(struct NS(_state) *s) {
    return read(0, (uint8_t*)s->in, sizeof(s->in));
}
int main(int argc, char **argv) {
    struct NS(_state) s = {};
    return NS(_loop)(&s);
}

#undef NS
