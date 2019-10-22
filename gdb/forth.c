#include "forth.h"
#include "cbuf.h"

/* Output needs to be buffered for USB polling. */
uint8_t     forth_out_buf[64];
struct cbuf forth_out;

uint32_t forth_read(uint8_t *buf, uint32_t size) {
    return cbuf_read(&forth_out, buf, size);
}
void forth_put(uint8_t byte) {
    cbuf_write(&forth_out, &byte, 1);
}
void forth_write(const uint8_t *buf, uint32_t len) {
    while(len--) forth_put(*buf++);
}
void forth_start(void) {
    CBUF_INIT(forth_out);
    const uint8_t hello[] = "hello forth\n";
    cbuf_write(&forth_out, hello, sizeof(hello)-1);
}


