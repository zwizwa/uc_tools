#ifndef MMAP_FILE
#define MMAP_FILE

/* A sane interface for memory-mapped read/write files: growable arrays. */

#define _GNU_SOURCE // for mremap()
#include "assert_mmap.h"
#include "assert_write.h"
#include <stdint.h>

struct mmap_file {
    void *buf;
    off_t size;
    int fd;
};

#ifndef MMAP_FILE_LOG
#define MMAP_FILE_LOG(...)
#endif

static inline void mmap_file_close(struct mmap_file *ref) {
    /* Idempotent. */
    if (!ref->buf) return;
    ASSERT_ERRNO(munmap(ref->buf, ref->size));
    ref->buf = NULL;
    ASSERT_ERRNO(close(ref->fd));
    ref->fd = -1;
}

static inline void mmap_file_sync(struct mmap_file *ref) {
    if (!ref->buf) return;
    ASSERT_ERRNO(msync(ref->buf, ref->size, MS_SYNC));
}

static inline off_t mmap_file_align(off_t size) {
    /* Can't map empty files. */
    if (!size) size = 1;

    /* Align requested size to page size. */
    const off_t page_size = 4096;
    return (((size-1)/page_size)+1)*page_size;
}

/* Precondition: file can be mapped, but file is open.
   Post condition: at least size bytes are available and file is mapped. */
static inline void *mmap_file_reserve(struct mmap_file *ref, off_t size) {
    size = mmap_file_align(size);
    if (size <= ref->size) {
        MMAP_FILE_LOG("backing file is large enough (%d <= %d)\n", size, ref->size);
        if (!ref->buf) {
            ref->buf = mmap(NULL, ref->size, PROT_READ | PROT_WRITE, MAP_SHARED, ref->fd, 0);
            ASSERT(MAP_FAILED != ref->buf);
        }
        return ref->buf;
    }

    MMAP_FILE_LOG("growing backing file to %d\n", size);
    ASSERT(size>0);
    off_t old_size = ref->size;
    ASSERT_ERRNO(lseek(ref->fd, size-1, SEEK_SET));
    uint8_t byte = 0;
    assert_write(ref->fd, &byte, 1);
    ref->size = size;
    if (!ref->buf) {
        ref->buf = mmap(NULL, ref->size, PROT_READ | PROT_WRITE, MAP_SHARED, ref->fd, 0);
    }
    else {
        ref->buf = mremap(ref->buf, old_size, ref->size, MREMAP_MAYMOVE);
    }
    ASSERT(MAP_FAILED != ref->buf);
    return ref->buf;
}

/* Post condition: at least size bytes are available and file is mapped. */
static inline void *mmap_file_open(struct mmap_file *ref,
                                   const char *file, off_t size) {
    memset(ref,0,sizeof(*ref));
    size = mmap_file_align(size);

    /* Open the file for read-write, create if necessary. */
    MMAP_FILE_LOG("opening %s\n", file);
    ASSERT_ERRNO(ref->fd = open(file, O_RDWR | O_CREAT, 0664));
    ASSERT_ERRNO(ref->size = lseek(ref->fd, 0, SEEK_END));

    return mmap_file_reserve(ref, size);
}

#endif
