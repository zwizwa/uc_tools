/* Index log files based on the uc_tools message format + memory
   mapped reader. */

#ifndef ILOG_H
#define ILOG_H

#include "macros.h"
#include "assert_write.h"
#include "uct_byteswap.h"
#include "fcntl.h"

struct ilog {
    /* File containing concatenated log messages. */
    int log_fd;
    /* File containing index = array of 64 bit file offsets. */
    int index_fd;
    uint64_t nb_bytes;
    uint64_t nb_messages;
};
struct ilog_read {
    struct ilog ilog;
    const uint64_t *index;
    const uint8_t *message;
};
static inline int ilog_open_fd(const char *filename, int flags) {
    return open(filename, flags, 0664);
}
static inline void ilog_open_with_flags(struct ilog *v,
                                        const char *basename, int flags) {
    memset(v, 0, sizeof(*v));
    char name[strlen(basename) + 6 + 1];

    sprintf(name, "%s.log", basename);
    ASSERT_ERRNO(v->log_fd = ilog_open_fd(name, flags));

    sprintf(name, "%s.index", basename);
    ASSERT_ERRNO(v->index_fd = ilog_open_fd(name, flags));
}
static inline void ilog_open_write(struct ilog *v, const char *basename) {
    int flags = O_WRONLY | O_TRUNC | O_CREAT;
    ilog_open_with_flags(v, basename, flags);
}
static inline void ilog_open_read(struct ilog_read *vr, const char *basename) {
    int flags = O_RDONLY;
    ilog_open_with_flags(&vr->ilog, basename, flags);
    uint64_t bs;

    /* Map the messages. */
    ASSERT_ERRNO(bs = lseek(vr->ilog.log_fd, 0, SEEK_END));
    vr->message = mmap(NULL, bs, PROT_READ, MAP_SHARED, vr->ilog.log_fd, 0);
    ASSERT(MAP_FAILED != vr->message);
    vr->ilog.nb_bytes = bs;

    /* Map the index. */
    ASSERT_ERRNO(bs = lseek(vr->ilog.index_fd, 0, SEEK_END));
    vr->index = mmap(NULL, bs, PROT_READ, MAP_SHARED, vr->ilog.index_fd, 0);
    ASSERT(MAP_FAILED != vr->index);
    vr->ilog.nb_messages = bs / sizeof(uint64_t); /* nb index messages */

}
static inline const uint8_t *ilog_get_message(struct ilog_read *vr, int i) {
    ASSERT(i >= 0);
    ASSERT(i < vr->ilog.nb_messages);
    uint64_t offset = vr->index[i];
    return vr->message + offset;
}


static inline void ilog_close(struct ilog *v) {
    close(v->log_fd);
    close(v->index_fd);
    v->log_fd = -1;
    v->index_fd = -1;
}
static inline uint64_t ilog_floats_fd(int fd, uint32_t cmd,
                                      const float *vec, uint32_t len) {
    ASSERT(fd != -1);
    uint32_t vec_bytes = sizeof(*vec) * len;
    uint8_t header[] = {
        U32_BE(vec_bytes + 8),
        U32_BE(0x1F320000), // TAG_FLOATS_ARRAY
        U32_BE(cmd),
    };
    uint32_t header_bytes = sizeof(header);
    assert_write(fd, (const void*)&header, header_bytes);
    assert_write(fd, (const void*)vec, vec_bytes);
    return header_bytes + vec_bytes;
}
static inline void ilog_write_index(struct ilog *v) {
    assert_write(v->index_fd, (const void*)&v->nb_bytes, sizeof(v->nb_bytes));

}
static inline void ilog_sync(struct ilog *v) {
    fdatasync(v->log_fd);
    fdatasync(v->index_fd);
}

static inline void ilog_floats(struct ilog *v, uint32_t cmd,
                               const float *vec, uint32_t len) {
    if (!v) return; // Allow NULL to disable log
    ilog_write_index(v);
    v->nb_bytes += ilog_floats_fd(v->log_fd, cmd, vec, len);
    v->nb_messages++;
    ilog_sync(v);
}


#endif
