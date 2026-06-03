/* Behaves like 'tee' but flips bits before writing.

   I did not find a good way to do this using other tools, so here's a
   new "place" that sits in between the sigrok.sh raw 8-bit logic
   output and a file.

   When the sigrok.sh output contains UART RX traces, the output will
   be 0xFF when all busses are idle.  I want to map this to sparse
   file support, e.g. flip the 0xFF bits to 0x00.

   This makes it possible to leave on the sniffer for a very long time
   before the file sizes become problematic, as the zero regions do
   not take up any file system blocks.

   It is straightforward enough to include a compensating XOR in any
   code that does analyis on the file.  I use this in a context where
   all events are indexed so there is never any scanning of the zero
   regions, just a convenient way to keep the file flat so it can be
   memory-mapped, since mmap() is also sparse, i.e. pages won't be
   loaded unless they are touched.
*/

#include "assert_write.h"
#include "macros.h"

uint8_t buf[2*1024*1024];

const char *getenv_default(const char *var, const char *dflt) {
    const char *val = getenv(var);
    if (!val) { val = dflt; }
    return val;
}

int main(int argc, char **argv) {
    if (argc != 2) { ERROR("usage: %s <file>", argv[0]); }
    int fd;
    ASSERT_ERRNO(fd = open(argv[1], (O_WRONLY | O_TRUNC | O_CREAT), 0664));
    uint8_t xor_mask = strtoul(getenv_default("XOR_MASK","0xFF"), NULL, 0);
    // LOG("xor_mask = 0x%x\n", xor_mask);
    for(;;) {
        ssize_t rv = read(0, buf, sizeof(buf));
        if (rv > 0) {
            assert_write_nb(1, buf, rv, 1000);
            for (ssize_t i=0; i<rv; i++) {
                buf[i] ^= xor_mask;
            }
            assert_write_nb(fd, buf, rv, 1000);
        }
        else if (rv == 0) {
            return 0;
        }
        else {
            perror("xor_tee");
            exit(1);
        }
    }
}
