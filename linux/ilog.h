/* Index log files based on the uc_tools message format + memory
   mapped reader. */

// TODO: Rebuild index if it is not present.

#ifndef ILOG_H
#define ILOG_H

#include "macros.h"
#include "assert_write.h"
#include "uct_byteswap.h"
#include "fcntl.h"

#include <inttypes.h>

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
    const uint64_t *index;   off_t index_size;
    const uint8_t *message;  off_t message_size;
};
static inline int ilog_open_fd(const char *filename, int flags) {
    return open(filename, flags, 0664);
}
static inline int ilog_is_stdout(const char *filename) {
    if (filename[0] != '-') return 0;
    if (filename[1] != 0) return 0;
    return 1;
}

static inline void ilog_open_with_flags(struct ilog *v,
                                        const char *basename, int flags) {
    memset(v, 0, sizeof(*v));
    char name[strlen(basename) + 6 + 1];

    sprintf(name, "%s", basename);
    ASSERT_ERRNO(v->log_fd = ilog_open_fd(name, flags));

    sprintf(name, "%s.index", basename);
    v->index_fd = ilog_open_fd(name, flags); // optional when reading
}
static inline void ilog_open_stdout(struct ilog *v) {
    memset(v, 0, sizeof(*v));
    v->log_fd = 1;
    v->index_fd = -1;
}

// FIXME: if basename is "-" then write to stdout.
static inline void ilog_open_write(struct ilog *v, const char *basename) {
    if (ilog_is_stdout(basename)) {
        ilog_open_stdout(v);
    }
    else {
        int flags = O_WRONLY | O_TRUNC | O_CREAT;
        ilog_open_with_flags(v, basename, flags);
        ASSERT_ERRNO(v->index_fd);
    }
}
static inline void ilog_open_read(struct ilog_read *vr, const char *basename) {
    int flags = O_RDONLY;
    ilog_open_with_flags(&vr->ilog, basename, flags);

    /* Map the messages. */
    ASSERT_ERRNO(vr->message_size = lseek(vr->ilog.log_fd, 0, SEEK_END));
    vr->message = mmap(NULL, vr->message_size, PROT_READ, MAP_SHARED, vr->ilog.log_fd, 0);
    ASSERT(MAP_FAILED != vr->message);
    vr->ilog.nb_bytes = vr->message_size;

    /* Map the index. */
    if (-1 == vr->ilog.index_fd) {
        vr->index = NULL;
        vr->index_size = 0;
    }
    else {
        ASSERT_ERRNO(vr->index_size = lseek(vr->ilog.index_fd, 0, SEEK_END));
        vr->index = mmap(NULL, vr->index_size, PROT_READ, MAP_SHARED, vr->ilog.index_fd, 0);
        ASSERT(MAP_FAILED != vr->index);
        vr->ilog.nb_messages = vr->index_size / sizeof(uint64_t); /* nb index messages */
    }
}
static inline const uint8_t *ilog_get(struct ilog_read *vr, int i) {
    ASSERT(i >= 0);
    ASSERT(i < vr->ilog.nb_messages);
    uint64_t offset = vr->index[i];
    return vr->message + offset;
}


static inline void ilog_close(struct ilog *v) {
    close(v->log_fd); // FIXME: Close stdout also?
    if (v->index_fd != -1) close(v->index_fd);
    v->log_fd = -1;
    v->index_fd = -1;
}
static inline uint64_t ilog_floats_fd(int fd, uint32_t cmd,
                                      const float *vec, uint32_t len) {
    ASSERT(fd != -1);
    // LOG("ilog_floats_fd %p %d\n", vec, len);
    uint32_t vec_bytes = sizeof(*vec) * len;
    uint8_t header[] = {
        U32_BE(vec_bytes + 8),
        U32_BE(0x1F320000), // TAG_FLOATS_ARRAY
        U32_BE(cmd),
    };
    uint32_t header_bytes = sizeof(header);
    assert_write(fd, (const void*)header, header_bytes);
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

static inline int64_t ilog_floats(struct ilog *v, uint32_t cmd,
                                  const float *vec, uint32_t len) {
    // Allow NULL to disable log and make that the fast path.
    if (likely(!v)) return -1;

    ilog_write_index(v);
    v->nb_bytes += ilog_floats_fd(v->log_fd, cmd, vec, len);
    uint64_t rv = v->nb_messages++;
    ilog_sync(v);
    return rv;
}

static inline uint64_t ilog_matrix_fd(int fd,
                                      const uint32_t *dims,
                                      const float *matrix) {
    ASSERT(fd != -1);
    // LOG("ilog_floats_fd %p %d\n", vec, len);
    uint32_t matrix_bytes = sizeof(*matrix) * dims[0] * dims[1];
    uint8_t header[] = {
        U32_BE(matrix_bytes + 12),
        U32_BE(0x1F320001), // TAG_FLOATS_MATRIX
        U32_LE(dims[0]),
        U32_LE(dims[1]),
    };
    uint32_t header_bytes = sizeof(header);
    assert_write(fd, (const void*)header, header_bytes);
    assert_write(fd, (const void*)matrix, matrix_bytes);
    return header_bytes + matrix_bytes;
}
static inline int64_t ilog_matrix(struct ilog *v,
                                  const uint32_t *dims,
                                  const float *matrix) {
    // Allow NULL to disable log and make that the fast path.
    if (likely(!v)) return -1;

    ilog_write_index(v);
    v->nb_bytes += ilog_matrix_fd(v->log_fd, dims, matrix);
    uint64_t rv = v->nb_messages++;
    ilog_sync(v);
    return rv;
}


/* Iteration.  Note that it doesn't make much sense to provide
   stateless functions in the absence of an allocator. */
typedef void (*ilog_zip_fn)(void*, const uint8_t *msg_a, const uint8_t *msg_b);
static inline int64_t ilog_zip(ilog_zip_fn f,
                               struct ilog_read *a,
                               struct ilog_read *b,
                               void *state) {
    uint64_t na = a->ilog.nb_messages;
    uint64_t nb = b->ilog.nb_messages;
    if (na != nb) {
        ERROR("na==%" PRIu64 " != nb=%" PRIu64 "\n", na, nb);
    }
    for (uint64_t i = 0; i<na; i++) {
        const uint8_t *msg_a = ilog_get(a, i);
        const uint8_t *msg_b = ilog_get(b, i);
        f(state, msg_a, msg_b);
    }
}

/* Specialized to matrix. */
typedef void (*ilog_matrix_zip_fn)(void *state, const uint32_t *dims, const float *a, const float *b);
struct ilog_matrix_zip_wrap {
    ilog_matrix_zip_fn f;  // user function
    void *state;           // user state
};
static inline void ilog_matrix_zip_wrap(void *state, const uint8_t *msg_a, const uint8_t *msg_b) {
    struct ilog_matrix_zip_wrap *wstate = state;
    uint32_t len_a = read_be(msg_a, 4); // Size is always present
    uint32_t len_b = read_be(msg_b, 4);
    ASSERT(len_a == len_b);
    uint32_t hdr_len = 4 * (1 + 2);
    ASSERT(len_a >=  hdr_len);
    ASSERT(0 == memcmp(msg_a+4, msg_b+4, hdr_len)); // Headers should be identicial
    ASSERT(0x1F320001 == read_be(msg_a+4, 4)); // TAG_FLOAT_MATRIX
    const uint32_t *dims = (void*) (msg_a + 8);
    const float *fa = (void*) (msg_a + 16);
    const float *fb = (void*) (msg_b + 16);
    wstate->f(wstate->state, dims, fa, fb);
}
static inline void ilog_matrix_zip(ilog_matrix_zip_fn f,
                                   struct ilog_read *a,
                                   struct ilog_read *b,
                                   void *state) {
    /* Header needs to be the same. */
    struct ilog_matrix_zip_wrap wstate = { .f = f, .state = state };
    ilog_zip(ilog_matrix_zip_wrap, a, b, &wstate);
}



#endif
