/* Format is:
   - data word
   - repeat count - 1 in LEB128 unsigned
*/

#include "assert_read.h"
#include <stdint.h>

typedef uint8_t data_t;
struct rle {
    uintptr_t count;
    data_t last;
};
static inline void rle_push(struct rle *s, data_t bus) {
    fwrite(&bus, 1, 1, stdout); // FIXME
}
static inline void rle_push_count(struct rle *s, uintptr_t sr) {
    for(;;) {
        if (sr < 128) {
            // fits -> last byte, no cont bit
            rle_push(s, sr);
            return;
        }
        else {
            // doesn't fit, write 7 LSB + cont bit
            rle_push(s,  0x80 | (sr & 0x7F));
            sr >>= 7;
        }
    }
}
void rle_push_data(struct rle *s, data_t bus) {
    for(uintptr_t i=0; i<sizeof(data_t); i++) {
        rle_push(s, bus);
        bus >>= 8;
    }
}
static inline void rle_push_bus(struct rle *s, data_t bus) {
    if(s->last != bus) {
        /* Flush count-1 */
        rle_push_count(s, s->count);
        rle_push_data(s, bus);
        s->count = 0;
        s->last = bus;
    }
    else {
        s->count++;
    }
}
void rle(struct rle *s, data_t *data, uintptr_t data_len) {
    for(uintptr_t i=0; i<data_len; i++) {
        rle_push_bus(s, data[i]);
    }
}

uintptr_t get_buf(data_t* buf, size_t n) {
    ssize_t rv = read(0, (uint8_t*)buf, n * sizeof(data_t));
    ASSERT(rv >= 0);
    return rv / sizeof(data_t);
}

data_t buf[1024*1024];

int main(int argc, char **argv) {
    struct rle s = {};
    /* Initial bus value */
    ASSERT(1 == get_buf(&s.last, 1));
    rle_push_data(&s, s.last);
    for(;;) {
        uintptr_t n = get_buf(buf, ARRAY_SIZE(buf));
        if (n == 0) {
            /* Last count */
            rle_push_count(&s, s.count);
            return 0;
        }
        rle(&s, buf, n);
    }
}
