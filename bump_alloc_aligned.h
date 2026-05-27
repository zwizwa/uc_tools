#ifndef BUMP_ALLOC_ALIGNED
#define BUMP_ALLOC_ALIGNED

#include <stdint.h>

// uintptr_t-aligned bump allocator
struct bump_alloc_aligned {
    uintptr_t *pool;
    uintptr_t size, next; // unit is nb uintptr_t slots
};
static inline void bump_alloc_aligned_init(struct bump_alloc_aligned *s,
                                           uintptr_t *pool,
                                           uintptr_t pool_words) {
    s->pool = pool;
    s->next = 0;
    s->size = pool_words;
}
static inline void *bump_alloc_aligned_alloc(struct bump_alloc_aligned *s,
                                             uintptr_t nb_bytes) {
    uintptr_t nb_words = 1 + ((nb_bytes-1)/sizeof(uintptr_t));
    uintptr_t room_words = s->size - s->next;
    ASSERT(nb_words <= room_words);
    void *mem = &s->pool[s->next];
    s->next += nb_words;
    return mem;
}

#endif
