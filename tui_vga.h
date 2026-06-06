/* VGA-style text frame buffer: array of char, attrib bytes.
   Unified interface for:
   i686/kernel.c
   emscripten/mod_tui_canvas.c
*/

#ifndef TUI_VGA_H
#define TUI_VGA_H

#include "tui.h"

struct tui_vga {
    uint8_t *video;
    uint8_t nb_rows;
    uint8_t nb_cols;
};


static inline uint8_t tui_vga_attrib(uint32_t fg, uint32_t bg) {
    return ((bg & 0xF) << 4) | (fg & 0xF);
}
static inline uint8_t tui_vga_win_attrib(struct tui_window *win) {
    return tui_vga_attrib(win->fg, win->bg);
}

static inline volatile uint16_t *tui_vga_u16(
    struct tui_vga *s, uint32_t x, uint32_t y) {
    return (volatile uint16_t*) (s->video + 2 * (y * s->nb_cols + x));
}


static inline void tui_vga_put(
    struct tui_vga *s,
    uint32_t x, uint32_t y, // absolute cordinates
    uint32_t c,   // character to write
    uint32_t fg,  // forground color
    uint32_t bg)  // background color
{
    volatile uint16_t *v = tui_vga_u16(s, x, y);
    *v = c | (tui_vga_attrib(fg, bg) << 8);
}


static inline void tui_vga_clear_line(
    struct tui_vga *s,
    uint32_t x, uint32_t y, // absolute coordinates
    uint32_t len,
    uint8_t attrib)
{
    volatile uint16_t *v = tui_vga_u16(s, x, y);
    uint16_t fill = ' ' + (attrib << 8);
    for (uint32_t i=0; i<len; i++) {
        v[i] = fill;
    }
}

static inline void tui_vga_clear(struct tui_vga *s, struct tui_window *win) {
    // x,y are window-relative coordinates.
    uint8_t attrib = tui_vga_win_attrib(win);
    for (uint32_t y=0; y<win->h; y++) {
        tui_vga_clear_line(s, win->x, win->y+y, win->w, attrib);
    }
}

static inline void tui_vga_copy_line(
    struct tui_vga *s,
    uint32_t x,
    uint32_t y,
    uint32_t len,
    int offset /* Copy from, >0 is down */
) {
    volatile uint16_t *dst = tui_vga_u16(s, x, y);
    volatile uint16_t *src = tui_vga_u16(s, x, y+offset);
    for (uint32_t i=0; i<len; i++) {
        dst[i] = src[i];
    }
}

static inline void tui_vga_scroll(struct tui_vga *s,
                                  struct tui_window *win,
                                  int lines) {
    if (lines > 0) { /* Scroll up */
        if (lines >= win->h) {
            /* Everything is scrolled away. */
            tui_vga_clear(s, win);
            return;
        }
        else {
            uint32_t nb_move = win->h - lines;
            /* Move the bottom of the window up.
               Start by moving the top line. */
            uint32_t y = win->y;
            for (uint32_t i = 0; i<nb_move; i++) {
                tui_vga_copy_line(s, win->x, y, win->w, lines);
                y++;
            }
            /* Clear the bottom of the window. */
            struct tui_window empty = *win;
            empty.y += empty.h - lines;
            empty.h  = lines;
            tui_vga_clear(s, &empty);
        }
    }
    else { /* Scroll down. */
        lines = -lines;
        if (lines >= win->h) {
            /* Everything is scrolled away. */
            tui_vga_clear(s, win);
            return;
        }
        else {
            uint32_t nb_move = win->h - lines;
            /* Move the bottom of top of the window down.
               Start by moving the bottom line. */
            uint32_t y = win->h-1-lines;
            for (uint32_t i = 0; i<nb_move; i++) {
                tui_vga_copy_line(s, win->x, y, win->w, -lines);
                y--;
            }
            /* Clear the top of the window. */
            struct tui_window empty = *win;
            empty.h = lines;
            tui_vga_clear(s, &empty);
        }
    }
}

#endif
