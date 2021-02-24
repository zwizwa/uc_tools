/* Implementation of the base functionality needed by infof.c
   FIXME: Change the name of this file to something meaningful. */

#if 0
// Stub for info calls.
int info_putchar(int c) { return 0; }
#else
// FIXME: just for bootstrapping some low-level code.
#define KEEP __attribute__((section(".keep")))
#include <stdint.h>
#include <string.h>
#define INFO_LOGSIZE 10
#define INFO_SIZE (1<<INFO_LOGSIZE)
#define INFO_MASK (INFO_SIZE-1)
uint8_t info_buf[INFO_SIZE];
uint32_t info_write_next = 0;
uint32_t info_read_next = 0;
KEEP void info_clear(void) {
    memset(info_buf,0,INFO_SIZE);
    info_write_next = 0;
    info_read_next = 0;
}
KEEP uint32_t info_bytes() {
    return info_write_next - info_read_next;
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
KEEP int info_putchar_inner(int c) {
#if 0 // A quick way to turn off logging
    return 0;
#else
    if (info_level_threshold > info_level_current) return 0;

    uint32_t room = INFO_SIZE - (info_write_next - info_read_next);
    if (room == 0) return 0;  // drop character and don't overflow the buffer
    if (room == 1) c = '?';   // when almost full, write some kind of indicator
    if (room <= 1) info_overflow_errors++;
    uint32_t offset = (info_write_next++) & INFO_MASK;
    info_buf[offset] = c;
    return 0;
#endif
}

/* Wrap info_putchar() with a hook called before printing the first
   character on a line. */
uint32_t info_last_was_newline = 1;
void (*info_newline_hook)(int (*putchar)(int c));
int info_putchar(int c) {
    if (info_last_was_newline) {
        if (info_newline_hook) {
            info_newline_hook(info_putchar_inner);
        }
    }
    info_last_was_newline = (c == '\n');
    return info_putchar_inner(c);
}

/* This is counterproductive: when overflow happens, it is usually due
 * to some runaway process.  In that case it is much more useful to
 * have a slight indicator of overrun, instead of dumping a huge
 * number of '?' characters.  So for now it is disabled.  TODO: A
 * little more elegance. */

KEEP uint32_t info_read(uint8_t *buf, uint32_t len) {
    uint32_t nb = 0;
    for(;;) {
        if (!len) return nb;
        int32_t todo = info_bytes();
        if (!todo) return nb;
        if (todo < INFO_SIZE) {
            *buf++ = info_buf[info_read_next & INFO_MASK];
            len--; nb++;
        }
        else {
            /* Not reached: overflows are preented at the write
             * end. */
            *buf++ ='?';
            len--; nb++;
        }
        info_read_next++;
    }
}
KEEP uint32_t info_read_crlf(uint8_t *buf, uint32_t len) {
    uint32_t nb = 0;
    for(;;) {
        if (len < 2) return nb;
        int32_t todo = info_bytes();
        if (!todo) return nb;
        uint8_t c = info_buf[info_read_next & INFO_MASK];
        if (todo < INFO_SIZE) {
            if (c == '\n') {
                *buf++ = '\r';
                len--; nb++;
            }
            *buf++ = c;
            len--; nb++;
        }
#if INFO_LOG_OVERFLOW
        else {
            *buf++ ='?'; // be verbose about buffer overrun
            len--; nb++;
        }
#endif
        info_read_next++;
    }
}
#endif
