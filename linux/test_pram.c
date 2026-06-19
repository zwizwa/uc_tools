#include "macros.h"
#define PRAM_LOG LOG
#include "persistent_sram.h"


/* Note that this test runs on 64 bit: the footer doesn't contain any
   pointers, so the data layout is the same. */

struct pram_slot s = {};
// uint8_t buf[1024] = {};
uint8_t buf[32] = {};
struct pram_footer *f = (void*)&buf[sizeof(buf)-sizeof(*f)];

void test(int expected_rv) {
    int rv =
        pram_check(&s,
                   &buf[0],
                   &buf[sizeof(buf)]);
    LOG("expected rv=%d, got rv=%d\n", expected_rv, rv);
    ASSERT(rv == expected_rv);
}

int main(int argc, char **argv) {
    // Provide input data for each of the error checks to fail as a
    // coverage test.

    f->slot_size = 0x12345678;
    test(PRAM_BAD_SLOT_SIZE);
    LOG("on slot size %d\n", f->slot_size);

    f->slot_size = sizeof(buf);
    f->data_len = 0x12345678;
    test(PRAM_BAD_DATA_LEN);

    f->data_len = 10;
    f->crc = 0x12345678;
    test(PRAM_BAD_SLOT_CRC);

    f->crc = crc32b(buf, sizeof(buf)-4);
    LOG("compute crc %08x from %p,%d\n", f->crc, buf, sizeof(buf)-4);
    test(PRAM_OK);

    memset(buf, 0x55, sizeof(buf));

    pram_seal(0x123,
              buf,
              10,
              sizeof(buf));
    test(PRAM_OK);
}

