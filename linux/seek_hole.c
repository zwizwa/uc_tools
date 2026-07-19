#include "macros.h"
int main(int argc, char *argv[])
{
    if (argc != 2) {
        ERROR("usage: %s <file>\n", argv[0]);
        return 1;
    }
    int fd;
    ASSERT_ERRNO(fd = open(argv[1], O_RDONLY));
    struct stat st;
    ASSERT_ERRNO(fstat(fd, &st));
    off_t end = st.st_size;

    off_t pos = 0;
    while (pos < end) {
        off_t data = lseek(fd, pos, SEEK_DATA);
        if (data < 0) {
            if (errno != ENXIO) {
                ERROR("lseek SEEK_DATA");
            }
            break;  /* ENXIO: only holes remain */
        }
        off_t hole = lseek(fd, data, SEEK_HOLE);
        if (hole < 0) {
            ERROR("lseek SEEK_HOLE");
            break;
        }
        long long hole_size = hole-data;
        LOG("data: %lld..%lld n=%lld\n",
            (long long)data, (long long)hole, hole_size);
        pos = hole;
    }

    close(fd);
    return 0;
}
