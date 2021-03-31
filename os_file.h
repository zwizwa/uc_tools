#ifndef OS_FILE_H
#define OS_FILE_H

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

#include "os_error.h"

/* Platform-specific. */
#if defined(FF_DEFINED) // FatFs


// FIXME: map error codes
#define OS_ERROR_NOT_FOUND ((os_error_t)(-10001))
#define OS_ERROR_IO        ((os_error_t)(-10002))



#define OS_FILE_READ  FA_READ
#define OS_FILE_WRITE (FA_WRITE|FA_CREATE_ALWAYS)

struct os_file {
    FIL f;
};
static inline os_error_t os_file_open(struct os_file *file, const char *filename, BYTE mode) {
    if (FR_OK != f_open(&file->f, filename, mode)) return OS_ERROR_NOT_FOUND;
    return OS_OK;
}
static inline os_result_t os_file_read(struct os_file *file, uint8_t *buf, uintptr_t size) {
    UINT nb_read = 0;
    if (FR_OK != f_read(&file->f, buf, size, &nb_read)) return os_error_result(OS_ERROR_IO);
    return os_ok_result(nb_read);
}
static inline os_error_t os_file_write(struct os_file *file, const uint8_t *buf, uintptr_t size) {
    UINT nb_written = 0;
    if (FR_OK != f_write(&file->f, buf, size, &nb_written)) return OS_ERROR_IO;
    if (nb_written != size) return OS_ERROR_IO;
    return OS_OK;
}
static inline os_error_t os_file_close(struct os_file *file) {
    if (FR_OK != f_close(&file->f)) return OS_ERROR_IO;
    return OS_OK;
}

// Default assumes stdio file API is available
#else
#include <stdio.h>


// FIXME: make a good mapping for these
#define OS_ERROR_NOT_FOUND ((os_error_t)(-10001))
#define OS_ERROR_IO_ERROR  ((os_error_t)(-10002))
#define OS_ERROR_EOF       ((os_error_t)(-10003))

struct os_file {
    FILE *f;
};
#define OS_FILE_READ "r"
#define OS_FILE_WRITE "w"
static inline os_error_t os_file_open(struct os_file *file, const char *filename, char *mode) {
    file->f = fopen(filename, mode);
    if (!file->f) return OS_ERROR_NOT_FOUND;
    return OS_OK;
}
static inline os_result_t os_file_read(struct os_file *file, uint8_t *buf, uintptr_t size) {
    intptr_t rv = fread(buf, 1, size, file->f);
    if (rv >= 0) return os_ok_result(rv);
    return os_ok_result(rv);
}
static inline os_error_t os_file_write(struct os_file *file, const uint8_t *buf, uintptr_t size) {
    int rv = fwrite(buf, 1, size, file->f);
    if (rv < 0) return os_error(rv);
    if (rv == size) return OS_OK;
    /* We consider EOF on write to be an error. */
    return OS_ERROR_EOF;
}
static inline os_error_t os_file_close(struct os_file *file) {
    fclose(file->f);
    return OS_OK;
}


#endif
#endif
