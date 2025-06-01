#ifndef RINGBUF_H
#define RINGBUF_H

/* Note: I eventually used a different approach.

   The idea was this: it makes sense in some cases to separate
   triggering / framing, and packet analysis.

   In such cases, when input is streaming, there is the need to either
   keep track of a window of data in a ringbuffer, or to use separate
   linear buffers to put the triggered packets for later analysis.

   I got stuck in a premature optimization trap, working from a vague
   idea that isn't consistent yet.  This is left here but should
   probably be removed if it doesn't proove to be viable.

   I think a better way is to:

   - Use raw data on-disk with mmap
   - Build indexing for that
   - Optimize storage at fileystem level (e.g. lz4 on zfs)

   Streaming is nice but in most cases not necessary and it
   complicates storage.

   FIXME: make this an ns container

*/

#include <stdint.h>
typedef uint8_t ringbuf_data_t;
struct ringbuf {
    uintptr_t mask;
    uintptr_t offset;
    struct ringbuf_data_t *buf;
};
static inline ringbuf_data_t ringbuf_next(struct ringbuf_t *s) {
    /* Refill criterion for reading from stdin. */
    // if() ...
    return s->buf[s->offset & s->mask];
}


#endif
