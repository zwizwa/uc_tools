#include "balloci.h"
int main(void) {
    uint32_t buf[64];
    struct balloci b = { .size = ARRAY_SIZE(buf), .buf = buf };
    balloci_clear(&b);
    /* Two index pointers are necessary: one to point to the object,
       and one to point to the sentinel. */
    uint32_t room = balloci_room(&b);
    ASSERT(room == ARRAY_SIZE(buf) - 2);

    uint32_t index[10] = {};

    for(int i=0;;i++) {
        uint32_t node = -1;
        LOG("\nattempt alloc %d\n", i);
        uint32_t *obj = balloci_alloc(&b, i, &node);
        if (!obj) {
            LOG("done\n");
            break;
        }
        uint32_t room_new = balloci_room(&b);
        /* Each subsequent allocation uses the number of words
           requested, plust one for the index. */
        ASSERT_EQ(room - room_new, i + 1);
        room = room_new;

        LOG("obj %d, node %d is at offset %d\n", i, node, obj - buf);

        ASSERT(node < ARRAY_SIZE(index));

        index[node] = obj-buf;

    }
    for (int i=0; i<b.count; i++) {
        uint32_t offset = balloci_index(&b, i) - buf;
        LOG("node %d is at offset %d\n", i, offset);
        ASSERT(index[i] == offset);
    }
}
