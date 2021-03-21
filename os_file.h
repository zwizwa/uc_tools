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

/* Negative are error codes. */
typedef intptr_t os_file_status_t;

#define OS_FILE_STATUS_OK        0
#define OS_FILE_STATUS_NOT_FOUND (-1)
#define OS_FILE_STATUS_ERROR     (-2)



/* Platform-specific. */
#if defined(FF_DEFINED) // FatFs

#define OS_FILE_READ  FA_READ
#define OS_FILE_WRITE FA_WRITE

struct os_file {
    FIL f;
};
static inline os_file_status_t os_file_open(struct os_file *file, const char *filename, BYTE mode) {
    if (FR_OK != f_open(&file->f, filename, mode)) return OS_FILE_STATUS_NOT_FOUND;
    return OS_FILE_STATUS_OK;
}
static inline os_file_status_t os_file_read(struct os_file *file, uint8_t *buf, uintptr_t size) {
    UINT nb_read = 0;
    if (FR_OK != f_read(&file->f, buf, size, &nb_read)) return OS_FILE_STATUS_ERROR;
    return nb_read;
}
static inline os_file_status_t os_file_write(struct os_file *file, const uint8_t *buf, uintptr_t size) {
    UINT nb_written = 0;
    if (FR_OK != f_write(&file->f, buf, size, &nb_written)) return OS_FILE_STATUS_ERROR;
    return nb_written;
}
static inline os_file_status_t os_file_close(struct os_file *file) {
    if (FR_OK != f_close(&file->f)) return OS_FILE_STATUS_ERROR;
    return OS_FILE_STATUS_OK;
}

// Default assumes stdio file API is available
#else
#include <stdio.h>
struct os_file {
    FILE *f;
};
#define OS_FILE_READ "r"
#define OS_FILE_WRITE "w"
static inline os_file_status_t os_file_open(struct os_file *file, const char *filename, char *mode) {
    file->f = fopen(filename, mode);
    if (!file->f) return OS_FILE_STATUS_NOT_FOUND;
    return OS_FILE_STATUS_OK;
}
static inline os_file_status_t os_file_read(struct os_file *file, uint8_t *buf, uintptr_t size) {
    int rv = fread(buf, 1, size, file->f);
    if (rv < 1) return 0;
    return rv;
}
static inline os_file_status_t os_file_write(struct os_file *file, const uint8_t *buf, uintptr_t size) {
    int rv = fwrite(buf, 1, size, file->f);
    if (rv < 1) return 0;
    return rv;
}
static inline os_file_status_t os_file_close(struct os_file *file) {
    fclose(file->f);
    return OS_FILE_STATUS_OK;
}


#endif
#endif
