/* Convert Logic2 CSV Export format to flat 8 channel binary at 2 MHz

   WAARNING: This has almost everything hardcoded, and will likely
   crash if format changes or if it is fed with something else. */

#include "macros.h"
#include "assert_mmap.h"  // read
#include "assert_write.h"
#include <stdint.h>
#include <stdlib.h>

#define BUF_SIZE_POW 20 // 1M
#define BUF_SIZE (1 << (BUF_SIZE_POW))
#define BUF_MASK (BUF_SIZE-1)

uint8_t buf[BUF_SIZE];

int main(int argc, const char **argv) {
    ASSERT(argc == 2);
    const char *in_name = argv[1];
    off_t size;
    const uint8_t *data = assert_mmap_rdonly(in_name, 0, &size);

    LOG("%s: %llu bytes\n", in_name, size);

    const uint8_t *endx = data + size;

    // Assumed one header line. Skip it.
    // Time [s],D,DE,RE,R,LAN D,LAN /R-DE,LAN R,Channel 7
    while (*data++ != 0x0a);

    // 0.000000000,1,0,0,1,1,0,1,1
    // 9.420631187,1,0,0,0,1,0,1,1

    uint64_t now = 0;
    uint8_t bus = 0;
    while (data < endx) {

        // Assumed time is in fractional seconds with 9 fractional
        // digits (nanoseconds).
        uint64_t time = 0;
        for(;;) {
            uint8_t c = *data++;
            if (c == ',') break;
            if (c == '.') continue;
            time = time * 10 + c - '0';
        }
        // Resample timestamp to 2MHz base
        time /= 500;

        // Read next bus data
        uint8_t next_bus = 0;
        for(;;) {
            uint8_t c = *data++;
            if (c == 0xa) break;
            if (c == ',') next_bus >>= 1;
            else next_bus |= ((1 & (c - '0')) << 7);
        }

        // FIXME: process
        // next_bus = (next_bus >> 4) & 1;


        // Convert to fixed rate stream
        while(now < time) {
            uint32_t buf_index = now & BUF_MASK;
            buf[buf_index] = bus;
            if (buf_index == BUF_MASK) {
                assert_write(1, buf, BUF_SIZE);
            }
            now++;
        }
        bus = next_bus;

        // LOG("%d %02x\n", time, bus);
    }

    // Flush tail
    uint32_t tail_size = now & BUF_MASK;
    if (tail_size > 0) {
        assert_write(1, buf, now & BUF_MASK);
    }

    return 0;
}
