#ifndef ASSERT_MMAP_H
#define ASSERT_MMAP_H

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
    LOG("mem = %p\n", mem);
    ASSERT(MAP_FAILED != mem);
    *psize = size;
    return mem;
}

#endif
