#ifndef MMAP_FILE
#define MMAP_FILE

/* A sane interface for memory-mapped read/write files: growable arrays. */
#include "assert_mmap.h"
#include "assert_write.h"


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

/* Internal: make sure file is large enough. */
static inline off_t mmap_file_grow__(struct mmap_file *ref, off_t size) {

    /* Can't map empty files. */
    if (!size) size = 1;

    if (size > ref->size) {
        /* Align requested size to page size. */
        const off_t page_size = sysconf(_SC_PAGESIZE);
        //MMAP_FILE_LOG("page_size = %d\n", page_size);
        size = (((size-1)/page_size)+1)*page_size;

        off_t old_size = ref->size;
        MMAP_FILE_LOG("growing: %d -> %d\n", old_size, size);
        ftruncate(ref->fd, size);
        ref->size = size;
        return old_size;
    }
    else {
        return -1;
    }
}

/* Precondition: file can be mapped, but file is open.
   Post condition: at least size bytes are available and file is mapped.
   Pointer can change. */
static inline void *mmap_file_reserve(struct mmap_file *ref, off_t size) {
    off_t old_size = mmap_file_grow__(ref, size);
    if (old_size < 0) return ref->buf; // no resize, so no remap
    ASSERT(ref->buf);
    ASSERT(ref->size);
    ref->buf = mremap(ref->buf, old_size, ref->size, MREMAP_MAYMOVE);
    ASSERT(MAP_FAILED != ref->buf);
    return ref->buf;
}

/* Post condition: at least size bytes are available and file is mapped. */
static inline void *mmap_file_open(struct mmap_file *ref,
                                   const char *file, off_t size) {
    memset(ref,0,sizeof(*ref));

    /* Open the file for read-write, create if necessary. */
    MMAP_FILE_LOG("opening %s\n", file);
    ASSERT_ERRNO(ref->fd = open(file, O_RDWR | O_CREAT, 0664));
    ASSERT_ERRNO(ref->size = lseek(ref->fd, 0, SEEK_END));

    /* Grow if necessary and map into memory. */
    mmap_file_grow__(ref, size);
    ref->buf = mmap(NULL, ref->size, PROT_READ | PROT_WRITE, MAP_SHARED, ref->fd, 0);
    ASSERT(MAP_FAILED != ref->buf);
    return ref->buf;
}

/* Post condition: at least size bytes are available and file is mapped. */
static inline const void *mmap_file_open_ro(struct mmap_file *ref,
                                            const char *file) {
    memset(ref,0,sizeof(*ref));

    /* Open the file for read-write, create if necessary. */
    MMAP_FILE_LOG("opening %s (ro)\n", file);
    ASSERT_ERRNO(ref->fd = open(file, O_RDONLY, 0664));
    ASSERT_ERRNO(ref->size = lseek(ref->fd, 0, SEEK_END));

    ref->buf = mmap(NULL, ref->size, PROT_READ, MAP_SHARED, ref->fd, 0);
    ASSERT(MAP_FAILED != ref->buf);
    return ref->buf;
}

static inline const void *mmap_file_open_rw(struct mmap_file *ref,
                                            const char *file,
                                            uintptr_t nb_bytes) {
    memset(ref,0,sizeof(*ref));

    /* Open the file for read-write, create if necessary. */
    MMAP_FILE_LOG("opening %s (rw)\n", file);
    ASSERT_ERRNO(ref->fd = open(file, O_RDWR | O_CREAT, 0664));
    ref->size = nb_bytes;
    ftruncate(ref->fd, nb_bytes);
    ref->buf = mmap(NULL, ref->size, PROT_READ | PROT_WRITE, MAP_SHARED, ref->fd, 0);
    ref->size = nb_bytes;
    ASSERT(MAP_FAILED != ref->buf);
    return mmap_file_reserve(ref, nb_bytes);
}

#endif
