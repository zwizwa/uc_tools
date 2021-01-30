#include "mod_sm_tags.c"
#include "macros.h"

int main(void) {
    /* This can run in {packet,4} */
    uint8_t buf[] = {
        U32_BE(12),
        U16_BE(TAG_U32), 0, 1,
        U32_BE(123),
        5,6,7,8
    };
    sm_tags_write(&sm_tags, buf, sizeof(buf));
}
