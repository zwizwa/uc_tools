#ifndef BLOCKING_IO_H
#define BLOCKING_IO_H

#include <stdint.h>

#include "os_error.h"


struct blocking_io;

/* Note that the return value here is not os_result_t but os_error_t,

   I.e. the call is guaranteed to transfer len bytes on success, so we
   don't need to check the transfer size. */

typedef os_error_t (*blocking_read_fn)(struct blocking_io *, uint8_t *buf, uintptr_t len);
typedef os_error_t (*blocking_write_fn)(struct blocking_io *, const uint8_t *buf, uintptr_t len);

/* Move this elsewhere. */
struct blocking_io {
    blocking_read_fn  read;
    blocking_write_fn write;
};


/* Define a default error handling mechanism.

   Typically in a single function body it is most convenient to use a
   single error variable, and a jump label for errors. */

#define BLOCKING_IO_CALL(_fun, _io, _buf, _len) do {                    \
        struct blocking_io *_io1 = (_io);                               \
        error = _io1->_fun(_io1, (_buf), (_len));                       \
        if (OS_OK != error) goto error_exit;                            \
    } while(0)

#define BLOCKING_IO_READ(_io, _buf, _len)       \
    BLOCKING_IO_CALL(read, _io, _buf, _len)
#define BLOCKING_IO_WRITE(_io, _buf, _len)      \
    BLOCKING_IO_CALL(write, _io, _buf, _len)

#endif
