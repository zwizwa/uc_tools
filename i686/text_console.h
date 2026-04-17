// PC style text console
#ifndef TEXT_CONSOLE_H
#define TEXT_CONSOLE_H

#include "hw_i686.h"

#include "tools.h"
#define memset mini_memset
#define memcpy mini_memcpy

struct text_console {
    volatile char *video;
    uint8_t col;
    uint8_t row;
    uint8_t nb_rows;
    uint8_t nb_cols;
    uint8_t attrib;
};
static inline void text_console_init(struct text_console *log) {
    memset(log, 0, sizeof(*log));
    log->attrib = 0x07;
    log->video = (volatile char *)0xB8000;
    log->nb_cols = 80;
    log->nb_rows = 25;
    /* Start at the bottom to cause a scroll at first character writen. */
    log->col = 0;
    log->row = log->nb_rows;
}

static inline void text_console_scroll(struct text_console *log) {
    // Note: mini_memcpy that allows backwards overlapping copy.
    uint32_t row_size     = 2 * log->nb_cols;
    uint32_t rows_m1_size = row_size * (log->nb_rows - 1);
    mini_memcpy_volatile(
        log->video,
        (void*)(log->video + row_size),
        rows_m1_size);
    mini_memset_volatile(
        log->video + rows_m1_size,
        0,
        row_size);
}
static inline uint32_t text_console_offset(struct text_console *log) {
    return 2 * (log->nb_cols * log->row + log->col);
}
static inline void text_console_putchar(struct text_console *log, char c) {
    while (log->row >= log->nb_rows) {
        text_console_scroll(log);
        log->col = 0;
        log->row--;
    }
    if (c == '\n') {
        // move to new line
        log->col = 0;
        log->row++;
    }
    else {
        typeof (log->video) v = log->video + text_console_offset(log);
        *v++ = c;
        *v++ = log->attrib;
        log->col++;
    }
    if (log->col == log->nb_cols) {
        // wrap end-of-line
        log->col = 0;
        log->row++;
    }
}
static inline void text_console_putstr(struct text_console *log, char *str) {
    while (*str) {
        text_console_putchar(log, *str++);
    }
}
static inline void text_console_clear(struct text_console *log) {
    for (int i = 0; i < log->nb_cols*log->nb_rows; i++) {
        log->video[i*2] = '.';
        log->video[i*2+1] = 0x0F; // white on black
    }
}


#endif
