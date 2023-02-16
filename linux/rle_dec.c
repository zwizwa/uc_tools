/* Format is:
   - data word
   - repeat count - 1 in LEB128 unsigned
*/

#include "assert_read.h"
#include <stdint.h>

typedef uint8_t data_t;

int main(int argc, char **argv) {
    /* FIXME: simpler with memmap, but that requires a file, i.e. this
     * can't be streamed from lz4 output. */
}
