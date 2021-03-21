#ifndef BLOCKING_IO_H
#define BLOCKING_IO_H

#include <stdint.h>

struct blocking_io;

typedef intptr_t (*blocking_read_fn)(struct blocking_io *, uint8_t *buf, uintptr_t len);
typedef intptr_t (*blocking_write_fn)(struct blocking_io *, const uint8_t *buf, uintptr_t len);

/* Move this elsewhere. */
struct blocking_io {
    blocking_read_fn  read;
    blocking_write_fn write;
};

#endif
