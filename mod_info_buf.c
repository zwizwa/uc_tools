#ifndef MOD_INFO_BUF
#define MOD_INFO_BUF

#if 1
/* FIXME: The default ABORT from macros.h uses this.

   It could be put in another file.  It just needs to be in the
   library, so library code can be made to abort in an app-specific
   way.  The default implementation is an infinite loop, which then
   can be pre-empted by a watchdog timer, or the abort_cb can be set
   to install an abort handler at run time. */
void (*abort_callback)(void);

void abort_busyloop(void) { 
    if (abort_callback) abort_callback();
  loop:
    goto loop;
}
#endif

/* Implementation of the base functionality needed by infof.c */

#include "info_buf.h"

#ifndef INFO_LOGSIZE
#define INFO_LOGSIZE 10
#endif

#define INFO_SIZE (1 << INFO_LOGSIZE)
#define INFO_MASK (INFO_SIZE - 1)

struct info_buf {
    struct info_buf_hdr hdr;
    uint8_t buf[INFO_SIZE];
};

#ifndef likely
#define likely(x)      __builtin_expect(!!(x), 1)
#endif


#if 0
// Stub for info calls.
int info_putchar(int c) { return 0; }
#else
// FIXME: just for bootstrapping some low-level code.
#define KEEP __attribute__((section(".keep")))
#include <stdint.h>
#include <string.h>
// Put it in a single object, for easy external debugger access
struct info_buf info_buf = { .hdr = { .logsize = INFO_LOGSIZE } };


KEEP void info_clear(void) {
    memset(&info_buf,0,sizeof(info_buf));
    info_buf.hdr.logsize = INFO_LOGSIZE;
    info_buf.hdr.write_next = 0;
    info_buf.hdr.read_next = 0;
}
KEEP uint32_t info_bytes() {
    return info_buf.hdr.write_next - info_buf.hdr.read_next;
}

/* This is a quick hack to implement log levels.  Two variables are
   used.  'threshold' sets the global log level, which gates
   info_putchar_inner.  'current' sets the threshold of the current
   logging context, which is modified by statements that support
   level-based logging, and is put back to 0 after the extent of the
   log call.

   Practically, too much needs to change to implement the threshold as
   context that is passed down the calls. The info mechanism was
   already non-reentrant, so this seems ok as a temporary hack.
   FIXME: Evaluate this later.
*/

/* Zero means default behavior: log everything. */
int32_t info_level_threshold = 0;
int32_t info_level_current = 0;

uint32_t info_overflow_errors = 0;

void info_mark_overflow(uint32_t room) {
    info_overflow_errors++;
    if (room) {
        /* Write an indicator that there was an overflow.  FIXME: Is
           it better not to duplicate these?  It is possible that 0xFF
           is part of a previous binary message. */
        uint32_t offset = (info_buf.hdr.write_next++) & INFO_MASK;
        info_buf.buf[offset] = 0xFF;
    }
}

static inline void __attribute__((always_inline)) info_write_byte_no_check(uint8_t b) {
    uint32_t offset = (info_buf.hdr.write_next++) & INFO_MASK;
    info_buf.buf[offset] = b;
}

#if 1 // old behavior: keep oldest
KEEP int info_write(uint8_t *buf, uintptr_t size) {
    /* Do one check, then write the buffer if it fits in a tight loop. */
    uint32_t room = INFO_SIZE - (info_buf.hdr.write_next - info_buf.hdr.read_next);
    if (likely(room > size)) {
        for(uintptr_t i=0; i<size; i++) {
            info_write_byte_no_check(buf[i]);
        }
    }
    else {
        info_mark_overflow(room);
    }
    return 0;
}
#else // new behavior: keep newest, which is most useful for crash logs
KEEP int info_write(uint8_t *buf, uintptr_t size) {
    uint32_t room = INFO_SIZE - (info_buf.hdr.write_next - info_buf.hdr.read_next);
    if (room < size) {
        // Discard old logs
        uint32_t to_discard = size - room;
        info_buf.hdr.read_next += to_discard;
    }
    // Now that there's room, write message.
    for(uintptr_t i=0; i<size; i++) {
        info_write_byte_no_check(buf[i]);
    }
    return 0;
}
#endif

/* This might not be necessary. */
int info_bin(uint32_t stamp, uint8_t *buf, uintptr_t size) {
    /* Do one check, then write the buffer if it fits in a tight loop. */
    uint32_t room = INFO_SIZE - (info_buf.hdr.write_next - info_buf.hdr.read_next);
    if (likely(room > (size + 5)) && (size <= 127)) {
        info_write_byte_no_check(0x80 + size);
        const uint8_t *s = (void*)&stamp;
        for(uintptr_t i=0; i<4; i++) {
            info_write_byte_no_check(s[i]);
        }
        for(uintptr_t i=0; i<size; i++) {
            info_write_byte_no_check(buf[i]);
        }
    }
    else {
        info_mark_overflow(room);
    }
    return 0;
}



KEEP int info_putchar_raw(int c) {
#if 0 // A quick way to turn off logging
    return 0;
#else
    if (info_level_threshold > info_level_current) return 0;
    uint8_t buf = c;
    return info_write(&buf, 1);
#endif
}

/* Wrap info_putchar() with a hook called before printing the first
   character on a line. */
uint32_t info_last_was_newline = 1;
void (*info_newline_hook)(int (*putchar)(int c));
int info_putchar(int c) {
    if (info_last_was_newline) {
        if (info_newline_hook) {
            info_newline_hook(info_putchar_raw);
        }
    }
    info_last_was_newline = (c == '\n');
    return info_putchar_raw(c);
}

KEEP uint32_t info_read(uint8_t *buf, uint32_t len) {
    uint32_t nb = 0;
    for(;;) {
        if (!len) return nb;
        int32_t todo = info_bytes();
        if (!todo) return nb;
        if (todo < INFO_SIZE) {
            *buf++ = info_buf.buf[info_buf.hdr.read_next & INFO_MASK];
            len--; nb++;
        }
        info_buf.hdr.read_next++;
    }
}

/* Ignore the read head and get all past log data up to write head.
   Do not update any pointers.  This is used for crash logs. */
void info_read_history(uint8_t *buf, uint32_t len) {
    uint32_t start = info_buf.hdr.write_next - len;
    for(uint32_t i = 0; i<len; i++) {
        *buf++ = info_buf.buf[(start + i) & INFO_MASK];
    }
}

KEEP uint32_t info_read_crlf(uint8_t *buf, uint32_t len) {
    uint32_t nb = 0;
    for(;;) {
        if (len < 2) return nb;
        int32_t todo = info_bytes();
        if (!todo) return nb;
        uint8_t c = info_buf.buf[info_buf.hdr.read_next & INFO_MASK];
        if (todo < INFO_SIZE) {
            if (c == '\n') {
                *buf++ = '\r';
                len--; nb++;
            }
            *buf++ = c;
            len--; nb++;
        }
        info_buf.hdr.read_next++;
    }
}
#endif
#endif
