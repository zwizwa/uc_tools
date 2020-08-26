#ifndef BUMP_ALLOC_H
#define BUMP_ALLOC_H

/* Bump allocator with separate run time and compile time addresses,
   e.g. for preparing Flash blocks in SRAM before writing. */

/* Assume output buffer is large enough, that input data is consistent
   (e.g. zero-terminated strings), and make sure algnment follows what
   the the C compiler does.  Make sure all bytes that are allocated
   are actually written. */

struct bump_alloc {
    uintptr_t addr;
    void *buf;
    uintptr_t next;
    uintptr_t log2_align;
};
static inline uintptr_t bump_alloc(struct bump_alloc *a, uintptr_t req_nb_bytes) {
    uintptr_t offset = a->next;
    a->next += aligned_bytes(a->log2_align, req_nb_bytes);
    memset(a->buf + offset, 0, req_nb_bytes);
    return offset;
}
static inline void bump_alloc_write_u32(struct bump_alloc *a_ign, void *buf, uintptr_t val) {
    *((uintptr_t*)buf) = val;
}
static inline void bump_alloc_write_string(struct bump_alloc *a, void *buf, const char *str) {
    uintptr_t offset = bump_alloc(a, strlen(str) + 1);
    *((uintptr_t*)buf) = a->addr + offset;
    mini_strcpy(a->buf + offset, str);
}


#endif // BUMP_ALLOC_H
