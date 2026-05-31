#ifndef MOD_TUI_FRAMEBUFFER
#define MOD_TUI_FRAMEBUFFER

/* TUI protocol writing to framebuffer in terms of tui_put */

/* Note that this is currently only used as a backend for a
   mod_tui_client.c which will do the window id allocation, so the
   framebuffer backend should use 2 modes:
   - follow external ids
   - allocate local ids

   This decision should probably be made in the mod_tui_server.c
   handlers.  Maybe it is simple: the backend doesn't need to care
   where the window structs live, it just uses them.

   So TODO: implement _new_ _del_ in terms of the same stack mechanism
   as in mod_tui_client.c
*/



/* Global variables. */
int g_foreground = 7;
int g_background = 0;
int g_reverse_video = 0;

void tui_put_fb(int x, int y, int c) {
    if (g_reverse_video) {
        tui_put(x, y, c, g_background, g_foreground);
    } else {
        tui_put(x, y, c, g_foreground, g_background);
    }
}

void tui_string_at(tui_window_t *w,
                   int x, int y,
                   int width,
                   const char *str) {
    if (y >= w->h) return;
    char pad = 0;
    for (uint32_t i=0; i<width; i++) {
        if (i >= w->w) break;
        if (str[i] == 0) {
            // String padding starts.
            pad = ' ';
        }
        char c = pad ? pad : str[i];
        tui_put_fb(w->x + x +i,
                   w->y + y,
                   c);
    }
}

void tui_box(tui_window_t *w) {
    LOG("FIXME: tui_framebuffer: box\n");
}

void tui_scroll(tui_window_t *w, int lines) {
    LOG("FIXME: tui_framebuffer: scroll %d\n", lines);
}


void tui_clear(tui_window_t *win) {
    for (uint32_t c = 0; c < win->w; c++) {
        for (uint32_t r = 0; r < win->h; r++) {
            tui_put_fb(win->x + c,
                       win->y + r,
                       ' ');
        }
    }

}

void tui_reverse_video(tui_window_t *w, int mode) {
    // FIXME: this is per window
    g_reverse_video = !!mode;
}

#endif
