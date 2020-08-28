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

/* Assuming buf points to an entry. */
static inline const uint8_t *log_entry_for_line(
    const uint8_t *buf, uintptr_t len,
    void (*f)(void *ctx, const uint8_t *msg, uintptr_t len),
    void *ctx) {

    /* Iterate over lines until stop condition is reached, then return
       the offset of the entry, or 0 if there was an unexpected end of
       file, which returns NULL. */
    for(;;) {
        if (!len) return 0;
        /* At least one char at this point.  Check for the normal end
           condition, which is an empty line. */
        if (buf[0] == '\n') return &buf[1];
        /* There is a new line.  Scan for end of line and pass it to
           callback. */
        uintptr_t i = 0;
        for(;;) {
            /* Check for unexpected end of file.  We don't pass it to
               the callback in this case. */
            if (i >= len) return 0;
            if (buf[i] == '\n') break;
            i++;
        }
        /* By guarding for the empty line condition, it is guaranteed
           that i > 0 so progress occurs. */
        f(ctx, buf, i);
        buf += i;
        len -= i;
    }
}
