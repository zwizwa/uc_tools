#include "cbuf.h"
#include "pbuf.c"

#include "macros.h"

void test_cbuf(struct cbuf *c, uint32_t n) {
    uint8_t buf[n*2];
    memset(buf, 123, sizeof(buf));
    ASSERT(0 == cbuf_bytes(c));
    ASSERT(n - 1 == cbuf_room(c));
    /* Write is atomic and will fail. */
    ASSERT(0 == cbuf_write(c, buf, n*2));
    /* Buffer should be full after filling it up. */
    ASSERT(n-1 == cbuf_write(c, buf, cbuf_room(c)));
    ASSERT(0 == cbuf_room(c));
    ASSERT(n-1 == cbuf_read(c, buf, sizeof(buf)));
    if (n > 3) {
        for(int i=0; i<1000; i++) {
            uint8_t a[] = {1,2,3};
            ASSERT(3 == cbuf_write(c, a, sizeof(a)));
            ASSERT(n-3-1 == cbuf_room(c));
            ASSERT(3 == cbuf_read(c, buf, 3));
            ASSERT(n-1 == cbuf_room(c));
            ASSERT(0 == memcmp(a,buf,3));
        }
    }
}
void with_cbuf(uint32_t n, void (*test)(struct cbuf *, uint32_t)) {
    struct cbuf c; uint8_t c_buf[n];
    CBUF_INIT(c);
    test(&c, n);
}
int main(int argc, char **argv) {
    LOG("test_buf.c\n");
    with_cbuf(2, test_cbuf);
    with_cbuf(20, test_cbuf);
    with_cbuf(200, test_cbuf);
}
