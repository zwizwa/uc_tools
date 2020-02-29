#ifndef SLIPLIB_H
#define SLIPLIB_H

#include "slip.h"
#include <stdint.h>

struct slip_write_state;
typedef void (*slip_write_byte_fn)(struct slip_write_state *s, uint8_t);
typedef void (*slip_write_end_fn)(struct slip_write_state *s);

struct slip_write_state {
    /* Environment */
    slip_write_byte_fn byte;
    slip_write_end_fn  end;
    /* State */
    uint8_t last;
};

/* Syphon off a byte stream to a tagged slip output. */
uint32_t slip_read_tagged(
    uint16_t tag,
    uint32_t (*read)(uint8_t *buf, uint32_t len),
    uint8_t *buf, uint32_t room);

void slip_write_tagged(struct slip_write_state *s, const uint8_t *buf, uint32_t len);

#endif
