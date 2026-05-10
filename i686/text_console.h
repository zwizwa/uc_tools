// PC style text console
#ifndef TEXT_CONSOLE_H
#define TEXT_CONSOLE_H

#include "hw_i686_crtc.h"
#include "tools.h"
#include <strings.h>

struct text_console {
    volatile uint8_t *video;
    uint8_t col;
    uint8_t row;
    uint8_t nb_rows;
    uint8_t nb_cols;
    uint8_t attrib;
    uint8_t top_rows;
    uint8_t use_cli:1;
    uint8_t raw:1;
};
static inline void text_console_set_cursor(struct text_console *log) {
    uint16_t pos = log->row;
    pos *= log->nb_cols;
    pos += log->col;
    set_cursor_pos(pos);
}
static inline void text_console_clear(struct text_console *log) {
    log->row = 0;
    log->col = 0;
    for (int i = 0; i < log->nb_cols*log->nb_rows; i++) {
        log->video[i*2] = ' ';
        log->video[i*2+1] = log->attrib;
    }
    text_console_set_cursor(log);
}

#define VIDEO ((volatile uint8_t *)0xB8000)

static inline void text_console_clear_top(struct text_console *log) {
    volatile uint8_t *v = (void*)log->video;
    for (int i=0; i<log->nb_cols*log->top_rows; i++) {
        *v++ = ' ';
        *v++ = 0x17;
    }
}

static inline void text_console_init(struct text_console *log) {
    memset(log, 0, sizeof(*log));
    log->attrib = 0x07;
    log->video = VIDEO;
    log->nb_cols = 80;
    log->nb_rows = 25;
    log->top_rows = 1;
    text_console_clear_top(log);
    switch(2) {
    case 1:
        /* Start at the bottom to cause a scroll at first character writen. */
        log->col = 0;
        log->row = log->nb_rows;
        break;
    case 2:
        /* Get cursor from display registers.  This is also where bios
           stopped writing to screen right after booting. */
        uint16_t pos = get_cursor_pos();
        log->row = pos / log->nb_cols;
        log->col = pos % log->nb_cols;
        break;
    case 3:
        text_console_clear(log);
        break;
    }
}
static inline void text_console_scroll(struct text_console *log) {

    uint32_t row_size   = 2 * log->nb_cols;
    uint32_t bytes_top  = log->top_rows * row_size;
    uint32_t bytes_move = row_size * (log->nb_rows - 1 - log->top_rows);

    // Note: mini_memcpy that allows backwards overlapping copy.
    mini_memcpy_volatile(
        /* dst */ log->video + bytes_top,
        /* src */ log->video + bytes_top + row_size,
        bytes_move);
    volatile uint8_t *v = (void*)log->video + bytes_top + bytes_move;
    for (int i=0; i<log->nb_cols; i++) {
        *v++ = ' ';
        *v++ = log->attrib;
    }
}
static inline uint32_t text_console_offset(struct text_console *log) {
    return 2 * (log->nb_cols * log->row + log->col);
}
static inline void text_console_maybe_scroll(struct text_console *log) {
    while (log->row >= log->nb_rows) {
        text_console_scroll(log);
        log->col = 0;
        log->row--;
    }
}
static inline void text_console_putstr(struct text_console *log, char *str);

void reboot(void);
static inline void text_console_putchar_nocli(struct text_console *log, uint8_t c) {
    if (c == 0) {
        // ignore null
    }
    else if (c > 127) {
        // ignore non-standard control codes
    }
    else if (c == '\n') {
        if (!log->raw) {
            // insert carriage return
            log->col = 0;
        }
        // newline
        log->row++;
    }
    else if (c == '\r') {
        // carriage return
        log->col = 0;
    }
    else if (c == 8) {
        if (log->col > 0) {
            // erase previous character if not on first col
            log->col--;
            typeof (log->video) v = log->video + text_console_offset(log);
            *v++ = ' ';
            *v++ = log->attrib;
        }
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
    text_console_maybe_scroll(log);
    text_console_set_cursor(log);
}
static inline void text_console_putchar(struct text_console *log, uint8_t c) {
    /* This can still be used from interrupt handler and initial setup
       code which has interrupts off. */
    int ie =interrupts_enabled();
    if(ie) cli();
    text_console_putchar_nocli(log, c);
    if(ie) sti();
}
static inline void text_console_putstr(struct text_console *log, char *str) {
    while (*str) {
        text_console_putchar(log, *str++);
    }
}



#endif
