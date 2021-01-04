#ifndef BALLOCI_H
#define BALLOCI_H

#include "macros.h"
#include <stdint.h>

/* Bump allocator with linear index.
   Currently specialized to uint32_t. Generalize this into an ns_ module. */
struct balloci {
    uint32_t *buf;
    uint32_t size;  // nb words in buffer
    uint32_t count; // number of objects stored
};

/* Objects are stored in the front of the buffer, index at the end.
   balloci_offset(i), i<count is a valid object
   balloci_offset(count) is the free pointer. */

static inline uint32_t *balloci_offset(struct balloci *b, uint32_t index) {
    return &b->buf[b->size-1-index];
}

static inline void balloci_clear(struct balloci *b) {
    b->count = 0;
    *balloci_offset(b, 0) = 0; // sentinel
}
static inline uint32_t balloci_next(struct balloci *b) {
    return *balloci_offset(b, b->count);
}
static inline uint32_t balloci_room(struct balloci *b) {
    uint32_t obj_used   = balloci_next(b);
    uint32_t index_used = b->count + 1;  // objects + sentinel pointer
    uint32_t payload_room = b->size - (obj_used + index_used + 1 /* index */);
    return payload_room;
}
/* Returns NULL when the reference is invalid. */
static inline uint32_t *balloci_index(struct balloci *b, uint32_t index) {
    if (index > b->count) return 0;
    return &b->buf[*balloci_offset(b, index)];
}
static inline uint32_t *balloci_alloc(struct balloci *b, uint32_t nb_words, uint32_t *index) {
    uint32_t room = balloci_room(b);
    LOG("room = %d\n", room);
    if (nb_words > room) return 0;
    uint32_t offset = balloci_next(b);
    if (index) *index = b->count;
    uint32_t next = offset + nb_words;
    b->count++;
    *balloci_offset(b, b->count) = next;
    return &b->buf[offset];
}

/* Only the last element can be removed. */
static inline void balloci_drop(struct balloci *b) {
    if (b->count == 0) return;
    b->count--;
}

#endif
