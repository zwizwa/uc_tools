/* This is written for ARM Cortex3, which has atomic uint32_t
   read/write as long as the words are properly aligned, and no
   reordering of memory operations.

   This allows cbuf to be used as a lock-free queue.  I don't know
   what exactly I need to prove, but here are some arguments that
   ensure that reads and writes are consistent, i.e.:

   - the read/write pointers obtained by the reader always span a
     range of well-defined bytes.

   - the read/write pointers obtained by writer always span a range of
     discarded bytes.

   Conditions:

   - all pointers are strictly increasing.  this ensures that a
     pointer's motion has either been caught by the other end, or not.
     if it has moved but the motion was not noticed, an
     underestimation of the free space or full happens, but no
     inconsistency happens.  ( IMPLEMENTATION )

   - a single reader / single writer (multiple readers/writers violate
     atomic update). ( USE )

   - reads and writes are atomic: this ensures that the pointer read
     is always a value that has been written, not some mangled partial
     update. ( ARCHITECTURE )

   - reads and writes are ordered: e.g in the following cases:
     writer:  (write X) then (write Y)
     reader:  (read  Y) then (read  X)

     if (read Y) reads the value corresponding to (write Y) and not
     the previous one, then (read X) necessarily corresponds to the
     new value writen by (write X).
     ( ARCHITECTURE )

*/

/*  NS(_fat_element_t) is NS(_element_t) union NS(_none) */


static inline void NS(_init)(NS(_queue_t) *b, NS(_element_t) *buf, uint32_t size) {
    b->write  = 0;
    b->read   = 0;
    b->size   = size;
    b->buf    = buf;
}

/* Note: this was a power-of-two implementation using convenient
   rolling pointers, but that turns out to be too wasteful.  It is now
   implemented as read/write counters that stay within the boundaries
   of the buffer.  Care is taken to ensure correct rollover behavior.
   One byte in the buffer is sacrificed to distinguish between empty
   and full. */
static inline uint32_t NS(_elements3)(const NS(_queue_t) *b, uint32_t read, uint32_t write) {
    int32_t bytes = write - read;  // signed!!!
    if (bytes < 0) bytes += b->size;
    return bytes;
}
static inline uint32_t NS(_elements)(const NS(_queue_t) *b) {
    return NS(_elements3)(b, b->read, b->write);
}
// FIXME: is this - 1 still necessary?
static inline uint32_t NS(_room)(const NS(_queue_t) *b) {
    return b->size - 1 - NS(_elements)(b);
}
static inline int NS(_empty)(const NS(_queue_t) *b) {
    return 0 == NS(_elements)(b);
}
static inline int NS(_full)(const NS(_queue_t) *b) {
    return 0 == NS(_room)(b);
}
/* Precondition: offset is smaller than size. */
static inline uint32_t NS(_index_)(const NS(_queue_t) *b, uint32_t base, uint32_t offset) {
    uint32_t bo = base + offset;
    return bo >= b->size ? bo-b->size : bo;
}
static inline NS(_fat_element_t) NS(_peek)(const NS(_queue_t) *b, uint32_t offset) {
    if (offset >= NS(_elements)(b)) return NS(_none);
    return b->buf[NS(_index_)(b, b->read, offset)];
}
static inline void NS(_drop)(NS(_queue_t) *b, uint32_t nb_drop) {
    uint32_t bs = NS(_elements)(b);
    if (nb_drop > bs) { nb_drop = bs; }
    b->read = NS(_index_)(b, b->read, nb_drop);
}
static inline void NS(_clear)(NS(_queue_t) *b) {
    NS(_drop)(b, 0xFFFFFFFF);
}
static inline void NS(_update_watermark)(NS(_queue_t) *b) {
#if CBUF_WATERMARK
    uint32_t elements = NS(_elements)(b);
    if (elements > b->watermark) b->watermark = elements;
#endif
}

#if CBUF_INFO_OVERFLOW
#include "infof.h"
#endif


/* Write is a transaction.  Everything or nothing gets written. */
static inline uint32_t NS(_write)(NS(_queue_t) *b, const NS(_element_t) *buf, uint32_t len) {
    uint32_t read  = b->read;
    uint32_t write = b->write;
    uint32_t elements = NS(_elements3)(b, read, write);
    uint32_t room  = b->size - 1 - elements;
    if (len > room) {
#if CBUF_COUNT_OVERFLOW
        b->overflow++;
#endif
#if CBUF_INFO_OVERFLOW
        infof("ns_cbuf_write overflow %p %p %d %d\n", b, buf, len, room);
#endif
        return 0;
    }
    for (uint32_t i=0; i<len; i++) {
        b->buf[NS(_index_)(b, write, i)] = buf[i];
    }
    b->write = NS(_index_)(b, write, len);
    NS(_update_watermark)(b);
    return len;
}

/* Reads can't just be transactions as we don't know the size, so
 * reads always read the minimum of buffer space and bytes
 * available. */
static inline uint32_t NS(_read)(NS(_queue_t) *b, NS(_element_t) *buf, uint32_t len) {
    uint32_t read  = b->read;
    uint32_t write = b->write;
    uint32_t elements = NS(_elements3)(b, read, write);
    if (len > elements) len = elements;
    for (uint32_t i=0; i<len; i++) {
        buf[i] = b->buf[NS(_index_)(b, read, i)];
    }
    b->read = NS(_index_)(b, read, len);
    return len;
}
static inline NS(_fat_element_t) NS(_get)(struct cbuf *b) {
    uint8_t element = 0;
    if (1 == cbuf_read(b, &element, 1)) { return element; }
    else return NS(_none);
}
static inline void NS(_put)(NS(_queue_t) *b, NS(_element_t) element) {
    NS(_write)(b, &element, 1);
}

