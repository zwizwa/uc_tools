#ifndef MMAP_ARRAY
#define MMAP_ARRAY

/* Array storage backend.
   This provides access to a collection of on-disk byte arrays.
   Originally developed as the backend for a mip-map representation of time series.

   The main operation is a cached version of this:

   - given id and desired size, return a pointer to the memmapped
     region and perform any operation necessary to create the file,
     resize it to accomodate the desired size, and initialize any
     additional memory to zero.

   - given id, close and unmap

*/

#define _GNU_SOURCE
#include "assert_mmap.h"
#include "assert_write.h"
#include <stdint.h>

struct mmap_array {
    const char *fmt;
    uintptr_t id;
    void *buf;
    off_t size;
    int fd;
};

/* This initializes the reference, not the array itself. */
static inline void mmap_array_init(struct mmap_array *ref, const char *fmt, uintptr_t id) {
    ASSERT(fmt);
    memset(ref,0,sizeof(*ref));
    ref->fmt = fmt;
    ref->id = id;
}
/* Flush data to disk, unmap. */
static inline void mmap_array_close(struct mmap_array *ref) {
    /* Idempotent. */
    if (!ref->buf) return;
    ASSERT_ERRNO(munmap(ref->buf, ref->size));
    ref->buf = NULL;
    ASSERT_ERRNO(close(ref->fd));
    ref->fd = -1;
}
static inline void mmap_array_sync(struct mmap_array *ref) {
    if (!ref->buf) return;
    ASSERT_ERRNO(msync(ref->buf, ref->size, MS_SYNC));
}

void mmap_array_grow(struct mmap_array *ref, off_t size) {
    LOG("growing backing file to %d\n", size);
    ASSERT(size>0);
    ASSERT_ERRNO(lseek(ref->fd, size-1, SEEK_SET));
    uint8_t byte = 0;
    assert_write(ref->fd, &byte, 1);
    ref->size = size;
}

void *mmap_array_buf(struct mmap_array *ref, off_t size) {
    /* Align it to page size. */
    const off_t page_size = 4096;
    size = (((size-1)/page_size)+1)*page_size;

    if ((size <= ref->size) && ref->buf) {
        LOG("Buffer is present and large enough.\n");
        return ref->buf;
    }

    if ((size > ref->size) && ref->buf) {
        LOG("Buffer is present but not large enough.\n");
        off_t old_size = ref->size;
        mmap_array_grow(ref, size);
        ASSERT(MAP_FAILED != (ref->buf = mremap(ref->buf, old_size, ref->size, MREMAP_MAYMOVE)));
        return ref->buf;
    }
    ASSERT(!ref->buf);

    /* Open the file for read-write, create if necessary. */
    char name[100/*Arbitrary*/+strlen(ref->fmt)];
    snprintf(name, sizeof(name), ref->fmt, ref->id);
    LOG("opening %s\n", name);
    ASSERT_ERRNO(ref->fd = open(name, O_RDWR|O_CREAT, 0664));
    ASSERT_ERRNO(ref->size = lseek(ref->fd, 0, SEEK_END));

    /* Grow if necessary. */
    if (ref->size < size) mmap_array_grow(ref, size);

    /* Map it into memory. */
    ref->buf = mmap(NULL, ref->size, PROT_READ | PROT_WRITE, MAP_SHARED, ref->fd, 0);
    ASSERT(MAP_FAILED != ref->buf);
    return ref->buf;
}

#endif
