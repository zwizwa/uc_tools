#ifndef OSAL_H
#define OSAL_H

/* OS abstraction layer.

   Note that macros.h already contains some small abstraction layer to
   implement LOG.  The purpose of this file is for OS features such
   as:

   - Threads and synchronization

   - File access

   - Netowork sockets

   The C standard library and Posix are too reliant on dynamic memory
   allocation, so the API here exposes operations that defer memory
   management to the caller, e.g. for static or stack based
   allocation.
*/

#include <stdint.h>

/* Negative are error codes. */
typedef intptr_t osal_status_t;

#define OSAL_STATUS_OK        0
#define OSAL_STATUS_NOT_FOUND (-1)

#if defined(_CHIBIOS_RT_)


/* Platform-specific. */
#elif defined(__linux__)
#include <stdio.h>
struct osal_file {
    FILE *f;
};
#define OSAL_FILE_READ "r"
#define OSAL_FILE_WRITE "w"
static inline osal_status_t osal_file_open(struct osal_file *file, const char *filename, char *mode) {
    file->f = fopen(filename, mode);
    if (!file->f) return OSAL_STATUS_NOT_FOUND;
    return OSAL_STATUS_OK;
}
static inline osal_status_t osal_file_read(struct osal_file *file, uint8_t *buf, uintptr_t size) {
    int rv = fread(buf, 1, size, file->f);
    if (rv < 1) return 0;
    return rv;
}
static inline osal_status_t osal_file_write(struct osal_file *file, const uint8_t *buf, uintptr_t size) {
    int rv = fwrite(buf, 1, size, file->f);
    if (rv < 1) return 0;
    return rv;
}
static inline osal_status_t osal_file_close(struct osal_file *file) {
    fclose(file->f);
    return OSAL_STATUS_OK;
}



#else
#error Need OSAL
#endif


#endif
