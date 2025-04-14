#ifndef FILE_TOOLS_H
#define FILE_TOOLS_H

#include "macros.h"
#include <stdint.h>

/* Memory is allocated with malloc. */

static inline char *file_read_string(const char *filename) {
    /* Read bin file. */
    FILE *f;
    ASSERT(NULL != (f = fopen(filename, "r")));
    ASSERT(0 == fseek(f, 0, SEEK_END));
    long len = ftell(f);
    char *buf = malloc(len) + 1;
    ASSERT(buf);
    ASSERT(0 == fseek(f, 0, SEEK_SET));
    ASSERT(len == fread(buf, 1, len, f));
    buf[len] = 0;
    return buf;
}

/* Allocate on stack and run callback. */

typedef intptr_t (*file_read_fn)(void *ctx, uint8_t *, uintptr_t);
static inline intptr_t file_read_with(file_read_fn handle, void *ctx, const char *filename) {
    /* Read bin file. */
    FILE *f;
    if (NULL == (f = fopen(filename, "r"))) return -1;
    if (0 != fseek(f, 0, SEEK_END)) return -1;
    long len = ftell(f);
    uint8_t buf[len];
    if (0 != fseek(f, 0, SEEK_SET)) return -1;
    if (len != fread(buf, 1, len, f)) return -1;
    return handle(ctx, buf, len);
}



#endif
