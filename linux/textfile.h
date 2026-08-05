#ifndef TEXTFILE_H
#define TEXTFILE_H

#include "macros.h"
#include <dirent.h>
#include <ctype.h>

/* Read textfile into malloc() string or NULL if file is not readable. */
static inline char *textfile(const char *filename, size_t *len_out) {
    FILE *f = fopen(filename, "r");
    if (!f) return NULL;
    char *buf = NULL;
    size_t cap = 0;
    errno = 0;
    /* Don't use ftell on /sys nodes. */
    ssize_t len = getdelim(&buf, &cap, '\0', f);
    if (len < 0) {
        if (ferror(f)) { free(buf); fclose(f); return NULL; }
        // EOF on an empty file: getdelim returns -1 without allocating
        len = 0;
        if (!buf) buf = calloc(1, 1);
    }
    fclose(f);
    if (len_out) *len_out = (size_t)len;
    return buf;  // already NUL-terminated by getdelim
}
static inline char *textfile_n(const char *filename) {
    // Strip the last character.
    size_t len = 0;
    char *str = textfile(filename, &len);
    if (!str) return NULL;
    while(isspace(str[len-1])) {
        str[len-1]=0;
        if (len == 0) return str;
        len--;
    }
    return str;
}
static inline char *textfile_fmt2_n(const char *fmt,
                                    const char *top,
                                    const char *entry) {
    /* Because of the %_ interpolation characters, summing the lengths
       is more than enough space. */
    char filename[strlen(top) + strlen(fmt) + strlen(entry)];
    sprintf(filename, fmt, top, entry);
    return textfile_n(filename);
}


#endif
