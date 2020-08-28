#include "macros.h"
#include <stdint.h>

/* This can probably be done faster by using word access if there
   are no alignment issues.  For now let's keep it simple. */
static inline int log_entry_at(const uint8_t *buf) {
    return
        ((buf[0] == 'E') &&
         (buf[1] == 'n') &&
         (buf[2] == 't') &&
         (buf[3] == 'r') &&
         (buf[4] == 'y') &&
         (buf[5] == ':') &&
         (buf[6] == ' '));
}


/* Assuming buf points at an Entry, skip to find the next one. */
static inline const uint8_t *log_entry_next(const uint8_t *buf, uintptr_t len) {
    /* Wind past. */
    if (len < 7) return 0;
    buf+=7; len-=7;
    for(;;) {
        if (len < 7) return 0;
        if (log_entry_at(buf)) return buf;
        buf++; len--;
        if (len < 7) return 0;
    }
}
/* Assuming buf points to the start of the memory-mapped file, return
   the size of the header.  There are 3 cases: empty file, only
   header, and header + at least one entry. */
static inline uintptr_t log_entry_header(const uint8_t *buf, uintptr_t len) {
    if (len == 0) return 0;
    const uint8_t *next; if (!(next = log_entry_next(buf, len))) return len;
    return next - buf;
}

static inline uintptr_t log_entry_for(
    const uint8_t *buf, uintptr_t len,
    uintptr_t (*f)(void *ctx, const uint8_t *msg, uintptr_t len),
    void *ctx) {
    for(;;) {
        const uint8_t *next = log_entry_next(buf, len);
        uintptr_t status = f(ctx, buf, len);
        /* Allow early stop. */
        if (status) return status;
        if (!next) return 0;
        len -= (next - buf);
        buf = next;
    }
}


