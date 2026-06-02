#ifndef ASSERT_MMAP_H
#define ASSERT_MMAP_H

#include <unistd.h>
#include <sys/mman.h>
#include <stdint.h>
#include "macros.h"

static inline const void *assert_mmap_rdonly(
    const char *filename, off_t offset, off_t *psize) {

    int fd;
    ASSERT_ERRNO(fd = open(filename, O_RDONLY));
    off_t size = lseek(fd, 0, SEEK_END);
    ASSERT((off_t)-1 != size);
    //ASSERT_ERRNO(lseek(fd, 0, SEEK_SET));
    const void *mem = mmap(NULL, size - offset, PROT_READ, MAP_SHARED, fd, offset);
    // LOG("mem = %p, MAP_FAILED = %p\n", mem, MAP_FAILED);
    // LOG("mem = %p\n", mem);
    ASSERT(MAP_FAILED != mem);
    *psize = size;
    return mem;
}

static inline uintptr_t page_size(void) {
    return sysconf(_SC_PAGESIZE);
}

static inline uintptr_t align_page_size(uintptr_t size) {
    uintptr_t ps = page_size();
    return (size + ps - 1) & ~(ps - 1);
}

// For use with userfaultfd
static inline const void *assert_mmap_anonymous(off_t size) {
    size_t size_aligned = align_page_size(size);
    const void *mem = mmap(NULL, size_aligned, PROT_READ, MAP_PRIVATE | MAP_ANONYMOUS, -1, 0);
    ASSERT(MAP_FAILED != mem);
    return mem;
}



#endif
