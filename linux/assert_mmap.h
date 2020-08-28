#include "macros.h"
#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <sys/mman.h>

static inline const void *assert_mmap_rdonly(
    const char *filename, off_t offset, off_t *psize) {

    int fd;
    ASSERT_ERRNO(fd = open(filename, O_RDONLY));
    off_t size = lseek(fd, 0, SEEK_END);
    ASSERT((off_t)-1 != size);
    //ASSERT_ERRNO(lseek(fd, 0, SEEK_SET));
    const void *mem = mmap(NULL, size - offset, PROT_READ, MAP_SHARED, fd, offset);
    ASSERT(MAP_FAILED != mem);
    *psize = size;
    return mem;
}
