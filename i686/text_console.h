// PC style text console
#ifndef TEXT_CONSOLE_H
#define TEXT_CONSOLE_H

#include "hw_i686_crtc.h"
#include "tools.h"
#include <strings.h>

#include "tui_vga.h"

#define VIDEO ((volatile uint8_t *)0xB8000)

struct text_console {
    /* Memory-level operations are from mod_tui_vga.c */
    struct tui_vga vga;
    /* Current state */
    uint8_t col;
    uint8_t row;
    uint8_t use_cli:1;
    uint8_t raw:1;
    /* Main text window */
    struct tui_window win;
    struct tui_window status;
};
static inline void text_console_set_cursor(struct text_console *log) {
    uint16_t pos = log->row;
    pos *= log->vga.nb_cols;
    pos += log->col;
    set_cursor_pos(pos);
}
static inline void text_console_clear(struct text_console *log) {
    tui_vga_clear(&log->vga, &log->win);
    log->col = log->win.x;
    log->row = log->win.y;
    text_console_set_cursor(log);
}
static inline void text_console_clear_top(struct text_console *log) {
    tui_vga_clear(&log->vga, &log->status);
}

static inline void text_console_split(struct text_console *log,
                                      uint32_t top_rows) {
    /* Two regions are abstract as tui_window  */
    log->win.w  = log->vga.nb_cols;
    log->win.h  = log->vga.nb_rows - top_rows;
    log->win.x  = 0;
    log->win.y  = top_rows;
    log->win.fg = 7;
    log->win.bg = 0;

    log->status.w  = log->vga.nb_cols;
    log->status.h  = top_rows;
    log->status.x  = 0;
    log->status.y  = 0;
    log->status.fg = 7;
    log->status.bg = 7;

}

static inline void text_console_init(struct text_console *log) {
    memset(log, 0, sizeof(*log));
    log->vga.video = (void*)VIDEO;
    log->vga.nb_cols = 80;
    log->vga.nb_rows = 25;

    text_console_split(log, 1);
    text_console_clear_top(log);

    switch(2) {
    case 1:
        /* Start at the bottom to cause a scroll at first character writen. */
        log->col = 0;
        log->row = log->vga.nb_rows;
        break;
    case 2:
        /* Get cursor from display registers.  This is also where bios
           stopped writing to screen right after booting. */
        uint16_t pos = get_cursor_pos();
        log->row = pos / log->vga.nb_cols;
        log->col = pos % log->vga.nb_cols;
        break;
    case 3:
        text_console_clear(log);
        break;
    }
}
static inline void text_console_scroll(struct text_console *log) {
    tui_vga_scroll(&log->vga, &log->win, 1);
}
static inline uint32_t text_console_offset(struct text_console *log) {
    return 2 * (log->vga.nb_cols * log->row + log->col);
}
static inline void text_console_maybe_scroll(struct text_console *log) {
    // FIXME: This can now just do one scroll with multiple lines
    while (log->row >= log->vga.nb_rows) {
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
            // FIXME: implement in tui_vga_
            // erase previous character if not on first col
            log->col--;
            typeof (log->vga.video) v = log->vga.video + text_console_offset(log);
            *v++ = ' ';
            *v++ = tui_vga_win_attrib(&log->win);
        }
    }
    else {
        typeof (log->vga.video) v = log->vga.video + text_console_offset(log);
        *v++ = c;
        *v++ = tui_vga_win_attrib(&log->win);
        log->col++;
    }
    if (log->col == log->vga.nb_cols) {
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
