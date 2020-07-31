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
        uint8_t a[] = {1,2,3};
        for(int i=0; i<1000; i++) {
            ASSERT(3 == cbuf_write(c, a, sizeof(a)));
            ASSERT(n-3-1 == cbuf_room(c));
            ASSERT(3 == cbuf_read(c, buf, 3));
            ASSERT(n-1 == cbuf_room(c));
            ASSERT(0 == memcmp(a,buf,3));
        }
        ASSERT(3 == cbuf_write(c, a, sizeof(a)));
        ASSERT(3 == cbuf_bytes(c));
        cbuf_drop(c, n);
        ASSERT(0 == cbuf_bytes(c));
    }
}
void with_cbuf(uint32_t n, void (*test)(struct cbuf *, uint32_t)) {
    struct cbuf c; uint8_t c_buf[n];
    CBUF_INIT(c);
    test(&c, n);
}


// Duplicate the test above for a u32 buffer instance.
typedef struct {
    volatile uint32_t write;
    volatile uint32_t read;
    uint32_t size;
    volatile uint32_t *buf;
#ifdef CBUF_WATERMARK
    volatile uint32_t watermark;
#endif
#ifdef CBUF_COUNT_OVERFLOW
    volatile uint32_t overflow;
#endif
} cbuf32_queue_t;
typedef uint32_t cbuf32_element_t;
typedef uint32_t cbuf32_fat_element_t;
#define cbuf32_none (0xFFFFFFFF)
#define NS(name) CONCAT(cbuf32,name)
#include "ns_cbuf.h"
#undef NS


void test_cbuf32(cbuf32_queue_t *c, uint32_t n) {
    cbuf32_element_t buf[n*2];
    memset(buf, 123, sizeof(buf));
    ASSERT(0 == cbuf32_elements(c));
    ASSERT(n - 1 == cbuf32_room(c));
    /* Write is atomic and will fail. */
    ASSERT(0 == cbuf32_write(c, buf, n*2));
    /* Buffer should be full after filling it up. */
    ASSERT(n-1 == cbuf32_write(c, buf, cbuf32_room(c)));
    ASSERT(0 == cbuf32_room(c));
    ASSERT(n-1 == cbuf32_read(c, buf, sizeof(buf)));
    if (n > 3) {
        uint32_t a[] = {1,2,3};
        for(int i=0; i<1000; i++) {
            ASSERT(3 == cbuf32_write(c, a, ARRAY_SIZE(a)));
            ASSERT(n-3-1 == cbuf32_room(c));
            ASSERT(3 == cbuf32_read(c, buf, 3));
            ASSERT(n-1 == cbuf32_room(c));
            ASSERT(0 == memcmp(a,buf,3*sizeof(cbuf32_element_t)));
        }
        ASSERT(3 == cbuf32_write(c, a, ARRAY_SIZE(a)));
        ASSERT(3 == cbuf32_elements(c));
        cbuf32_drop(c, n);
        ASSERT(0 == cbuf32_elements(c));
    }
}
void with_cbuf32(uint32_t n, void (*test)(cbuf32_queue_t *, uint32_t)) {
    uint32_t c_buf[n];
    cbuf32_queue_t c = {.buf = c_buf, .size = n };
    test(&c, n);
}


int main(int argc, char **argv) {
    LOG("test_buf.c\n");
    with_cbuf(2, test_cbuf);
    with_cbuf(20, test_cbuf);
    with_cbuf(200, test_cbuf);

    with_cbuf32(2, test_cbuf32);
    with_cbuf32(20, test_cbuf32);
    with_cbuf32(200, test_cbuf32);
}
