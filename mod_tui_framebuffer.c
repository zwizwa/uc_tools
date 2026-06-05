#ifndef MOD_TUI_FRAMEBUFFER
#define MOD_TUI_FRAMEBUFFER

/* TUI protocol writing to framebuffer in terms of tui_put */

void tui_bg_fg(tui_window_t *win, uint32_t *bg, uint32_t *fg) {
    if (win->reverse_video) {
        *bg = win->fg;
        *fg = win->bg;
    }
    else {
        *fg = win->fg;
        *bg = win->bg;
    }
}
void tui_char_at(tui_window_t *win,
                 uint32_t x, uint32_t y,
                 uint32_t c) {
    // clip
    if (x >= win->w) return;
    if (y >= win->h) return;

    uint32_t bg, fg;
    tui_bg_fg(win, &bg, &fg);

    //LOG("char_at %d %d\n", win->x + x, win->y + y);

    tui_put(win->x + x,
            win->y + y,
            c,
            fg,
            bg);
}


void tui_string_at(tui_window_t *win,
                   uint32_t x, uint32_t y,
                   uint32_t width,
                   const char *str) {
    //LOG("string_at wid=%d (%d,%d) %d %s\n",win->id,x,y,width,str);
    //LOG("win at (%d,%d) size (%d,%d)\n",win->x,win->y,win->w,win->h);

    if (width == 0) width = strlen(str);

    if (y >= win->h) return;
    uint32_t bg, fg;
    tui_bg_fg(win, &bg, &fg);
    char pad = 0;
    for (uint32_t i=0; i<width; i++) {
        if (i >= win->w) break; // clip
        if (str[i] == 0) {
            // String padding starts.
            pad = ' ';
        }
        char c = pad ? pad : str[i];
        tui_put(win->x + x +i,
                win->y + y,
                c,
                fg,
                bg);
    }
}

void tui_box(tui_window_t *win) {
    // LOG("tui_box %d %d %d %x\n", win->x, win->y, win->w, win->h);
    if (win->w < 2) return;
    if (win->h < 2) return;
    /* We at least have cornes. */
    uint32_t w = win->w;
    uint32_t h = win->h;

    // tl tr br br hor ver
    // ASCII
    // const uint8_t box[6] = {'+','+','+','+','-','|'};
    // VGA Code Page 437 box characters
    const uint8_t box[6] = {0xDA,0xBF,0xC0,0xD9,0xC4,0xB3};

    tui_char_at(win, 0,   0,   box[0]);
    tui_char_at(win, w-1, 0,   box[1]);
    tui_char_at(win, 0,   h-1, box[2]);
    tui_char_at(win, w-1, h-1, box[3]);
    if (w > 2) {
        for(uint32_t x=1; x<w-1; x++) {
            tui_char_at(win, x, 0,   box[4]);
            tui_char_at(win, x, h-1, box[4]);
        }
    }
    if (w > 2) {
        for(uint32_t y=1; y<h-1; y++) {
            tui_char_at(win, 0,   y, box[5]);
            tui_char_at(win, w-1, y, box[5]);
        }
    }
}

void tui_scroll(tui_window_t *win, int lines) {
    canvas_scroll(win->x, win->y,
                  win->w, win->h,
                  lines);
}


void tui_clear(tui_window_t *win) {
    for (uint32_t c = 0; c < win->w; c++) {
        for (uint32_t r = 0; r < win->h; r++) {
            tui_put(win->x + c,
                    win->y + r,
                    ' ',
                    win->fg,
                    win->bg);
        }
    }

}

void tui_reverse_video(tui_window_t *w, int mode) {
    w->reverse_video = !!mode;
}

tui_window_t *tui_new_window(int width, int height, int x, int y) {
    struct tui_window *win = malloc(sizeof(*win));
    memset(win,0,sizeof(*win));
    // win->id is set by mod_tui_server
    win->w = width;
    win->h = height;
    win->x = x;
    win->y = y;
    win->fg = TUI_DEFAULT_FG;
    win->bg = TUI_DEFAULT_BG;
    return win;
}
void tui_del_window(struct tui_window *w) {
    memset(w,0,sizeof(*w));
    free(w);
}
void tui_update_screen(void) {
}


#endif
