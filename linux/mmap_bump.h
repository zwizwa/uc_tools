/* Bump allocator, useful for "render apps" where malloc/free is just
   too much effort and GC is too slow or cumbsersome.

   Initial idea was to combine this with some restrictions on data
   structures such that copies can happen automatically, but we're not
   doing that.  Better to just put copy constructors in the C code and
   be compatible with arbitrary contents.
*/


#ifndef MMAP_BUMP_H
#define MMAP_BUMP_H

#include <stddef.h>
#include <stdint.h>
#include <stdalign.h>
#include <sys/mman.h>
#include <string.h>

#include "macros.h"

#ifndef MMAP_BUMP_MAX_SIZE
#define MMAP_BUMP_MAX_SIZE (64ULL << 30) // 64 GB
#endif

#ifndef MMAP_BUMP_ALIGN
#define MMAP_BUMP_ALIGN 16
#endif

struct mmap_bump {
    uint8_t *pool;
    uintptr_t size;
    uintptr_t next;
};

static inline void mmap_bump_init(struct mmap_bump *s) {
    memset(s,0,sizeof(*s));
    // Reserve a large virtual range — no physical memory yet
    s->pool = mmap(NULL, MMAP_BUMP_MAX_SIZE, PROT_NONE, MAP_PRIVATE|MAP_ANONYMOUS, -1, 0);
    ASSERT(s->pool != MAP_FAILED);
    s->size = MMAP_BUMP_ALIGN*1024;
    s->next = 0;
    // Commit the initial chunk
    mprotect(s->pool, s->size, PROT_READ|PROT_WRITE);
}
static inline void *mmap_bump_alloc(struct mmap_bump *s, uintptr_t req_size) {
    void *next = s->pool + s->next;
    if (unlikely(req_size == 0)) return NULL;
    uintptr_t a = MMAP_BUMP_ALIGN;
    uintptr_t size = (((req_size-1)/a)+1)*a;
    LOG("%d %d\n", req_size, size);
    uintptr_t chunk_endx = s->next + size;
    while (unlikely(chunk_endx > s->size)) {
        uintptr_t extra = s->size;
        uintptr_t new_size = s->size + extra;
        ASSERT(new_size <= MMAP_BUMP_MAX_SIZE);
        // Keep commit explicit so we get hard faults outside of range instead of silent page mapping.
        mprotect(s->pool + s->size, extra, PROT_READ|PROT_WRITE);
        s->size = new_size;
    }
    s->next += size;
    return next;
}

static inline void mmap_bump_clear(struct mmap_bump *s) {
    s->next = 0;
    /* Discard phsyical pages.  On touch they will page in
       zero-filled.  The PROT_READ|PROT_WRITE stays intact. */
    madvise(s->pool, s->size, MADV_DONTNEED);
}


// https://claude.ai/chat/fbe9f9f5-1b23-4721-b173-33ea47984ca7
// Some notes about how this might run into limitations.  Probably not relevant for a while.
//
// https://claude.ai/chat/e18edc11-2014-4e6d-8531-a221b6713879
// More about the double buffering

/* Combine two mmap_bump regions in a single "arena" object that has a
   copy operation to pull data from one phase into the next pahse.
   All the other data is deteted after the copy. */

struct arena {
    struct mmap_bump current;
    struct mmap_bump next;
};

/* Start allocating in the other region without deleting the curreent
   data.  Do this before live set copy. */
static inline void arena_swap(struct arena *a) {
    struct mmap_bump tmp = a->current;
    a->current = a->next;
    a->next = tmp;
}
/* Delete data in the other region.  Do this after live set copy. */
static inline void arena_clear(struct arena *a) {
    mmap_bump_clear(&a->next);
}

static inline void *arena_alloc(struct arena *a, uintptr_t size) {
    return mmap_bump_alloc(&a->current, size);
}
static inline void *arena_alloc0(struct arena *a, uintptr_t size) {
    /* I think this is guarnteeed to be zeroed by the kernel.  Check that. */
    void *mem = arena_alloc(a, size);
    if (mem) memset(mem,0,size);
    return mem;
}
static inline void arena_init(struct arena *a) {
    mmap_bump_init(&a->current);
    mmap_bump_init(&a->next);
}

static inline char *arena_strdup(struct arena *a, const char *str) {
    uintptr_t n = strlen(str);
    char *rv = arena_alloc(a, n);
    memcpy(rv, str, n);
    return rv;
}


#endif
