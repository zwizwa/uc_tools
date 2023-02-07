/* Small tool for cropping / dividing logic traces.
   Works non-destructively: mmaps input file and dumps result on stdout */
#include "macros.h"
#include "assert_mmap.h"
#include <stdint.h>
#include <stdlib.h>

const uint8_t *data8;
off_t left = 0;
off_t right = 0;

void write_stdout(void) {
    /* write */
    ASSERT(left < right);
    size_t to_write = right - left;
    LOG("to_write = %llu\n", to_write);
    size_t written = fwrite(&data8[left], 1, to_write, stdout);
    ASSERT(written == to_write);
    fflush(stdout);
}


int main(int argc, char **argv) {
    const char *cmd = argv[1];
    ASSERT(argc >= 2);

    if (!strcmp(cmd, "crop")) {

        ASSERT(argc == 3);
        const char *in  = argv[2];

        off_t size;
        data8 = assert_mmap_rdonly(in, 0, &size);
        LOG("size = %llu\n", size);

        ASSERT(size > 0);
        left = 0;
        right = size - 1;

        if (1) {
            /* Use native word size to make it go faster.  It is
               assumed there is enough preamble / postamble to do
               this.  FIXME: Add special case to check first 8 bytes
               etc... */
            const uintptr_t *vdata = (const void*)data8;
            off_t vsize = size / sizeof(uintptr_t);
            ASSERT(vsize > 0);

            uintptr_t v0 = vdata[0];
            LOG("v0 = 0x%llx\n", v0);
            for(off_t i=1; i<vsize; i++) {
                if (vdata[i] != v0) {
                    left = i * sizeof(uintptr_t);
                    LOG("left = %llu\n", left);
                    break;
                }
            }

            uintptr_t v1 = vdata[vsize-1];
            LOG("v1 = 0x%llx\n", v1);
            for(off_t i=vsize-1; i>0; i--) {
                if (vdata[i] != v1) {
                    right = i * sizeof(uintptr_t);
                    LOG("right = %llu\n", right);
                    break;
                }
            }
            write_stdout();
        }
    }
    else {
        ERROR("unknown cmd %s\n", cmd);
    }

    return 0;
}
