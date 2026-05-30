#ifndef MOD_ILOG_BROWSER
#define MOD_ILOG_BROWSER

/* This should support two interfaces:
   - plain ncurses
   - the emscripten browser canvas websocket thing

   This uses the minimal mod_tui interface.  Note that we use it in a
   blocking way.  On kernel.c and bare metal uc it will be necessary
   to implement the controller as event-driven code.
*/

#ifndef MOD_TUI
#include "mod_tui_ncurses.c"
#endif

#include "ilog.h"

struct ilog_browser {
    int info_h;        // hight of info window, including border
    int list_h;        // light of message list window
    tui_window_t *list_w;
    tui_window_t *info_w;
    int rows;          // number of visible list rows
    int sel;           // selected message index
    int top;           // top row message index
    struct ilog_read ilog;
};

// Defined outside this module.
void ib_format_message(struct ilog_browser *s, int index, char *buf, int max_chars);

int ib_nb_items(struct ilog_browser *s) {
    return s->ilog.ilog.nb_messages;
}

// render message on the correct row, highlighting on/off
void ib_draw_row(struct ilog_browser *s, int index, int highlight) {
    int y = (index) - s->top;
    char buf[tui_cols()];
    ib_format_message(s, index, buf, sizeof(buf));
    int x = 0;
    tui_reverse_video(s->list_w, highlight);
    tui_string_at(s->list_w, x, y, tui_cols()-1, buf);
    tui_reverse_video(s->list_w, 0);
}

// redraw all messages in the message window
void ib_redraw_list(struct ilog_browser *s) {
    tui_clear(s->list_w);
    for (int i = s->top; i < s->top + s->rows && i < ib_nb_items(s); i++) {
        ib_draw_row(s, i, i == s->sel);
    }
}

// draw message info
void ib_redraw_info(struct ilog_browser *s) {
    tui_clear(s->info_w);
    tui_box(s->info_w);
    char buf[100];
    snprintf(buf, sizeof(buf), "Selected: index %d", s->sel);
    tui_string_at(s->info_w, 2, 1, 0, buf);
}


// initialize screen, handle events, restore screen
// exits on 'q' or SIGWINCH
int ib_event_loop(struct ilog_browser *s) {

    int restart = 0;

    tui_init_screen();

    // split screen: message window on top, info window on bottom
    s->list_h = tui_lines() - s->info_h;
    s->list_w = tui_new_window(tui_cols(), s->list_h, 0, 0);
    s->info_w = tui_new_window(tui_cols(), s->info_h, 0, s->list_h);

    s->rows  = s->list_h;      // visible list rows

    ib_redraw_list(s);
    ib_redraw_info(s);

    tui_update_window(s->list_w);
    tui_update_window(s->info_w);
    tui_update_screen();

    for(;;) {
        int ch = tui_get_event(s->list_w);
        int old = s->sel;
        int last = ib_nb_items(s)-1;

        /* Control. */
        if (ch == TUI_RESIZED) {
            restart = 1;
            break;
        }
        if (ch == TUI_ERR) {
            restart = 0;
            break;
        }
        if (ch == 'q') {
            restart = 0;
            break;
        }

        /* Regular keys. */
        if (ch == TUI_KEY_DOWN && s->sel < last) {
            s->sel++;
        }
        else if (ch == TUI_KEY_UP && s->sel > 0) {
            s->sel--;
        }
        else if (ch == TUI_KEY_NPAGE) {
            s->sel += s->rows;
            if (s->sel > last) {
                s->sel = last;
            }
        }
        else if (ch == TUI_KEY_PPAGE) {
            s->sel -= s->rows;
            if (s->sel < 0) {
                s->sel = 0;
            }
        }
        else if (ch == TUI_KEY_HOME) {
            s->sel = 0;
        }
        else if (ch == TUI_KEY_END) {
            s->sel = last;
        }

        //  else if (ch == TUI_KEY_F(1)) { handle_f1(...); }
        else {
            // other keys don't update layout
            continue;
        }

        // Has the selection scrolled off the visible window? */
        if (s->sel < s->top) {
            int delta = s->top - s->sel;       // number of rows to scroll back
            s->top = s->sel;
            if (delta == 1) {                  // single step: hardware scroll
                tui_scroll(s->list_w, -1);
                ib_draw_row(s, s->sel, 1);     // paint the row that scrolled in
                ib_draw_row(s, old, 0);        // un-highlight old (if visible)
            } else {
                ib_redraw_list(s);             // page jump: just redraw everything
            }
        }
        else if (s->sel >= s->top + s->rows) {
            int delta = s->sel - (s->top + s->rows - 1);
            if (delta == 1) {
                ib_draw_row(s, old, 0);        // un-highlight while top is still old value
                s->top = s->sel - s->rows + 1;
                tui_scroll(s->list_w, 1);
                ib_draw_row(s, s->sel, 1);     // new bottom row
            } else {
                s->top = s->sel - s->rows + 1;
                ib_redraw_list(s);
            }
        }
        else {
            // still on screen: only the two changed rows need redrawing
            ib_draw_row(s, old, 0);
            ib_draw_row(s, s->sel, 1);
        }

        ib_redraw_info(s);
        tui_update_window(s->list_w);
        tui_update_window(s->info_w);
        tui_update_screen();  // one flush, no flicker
    }

    tui_del_window(s->list_w);
    tui_del_window(s->info_w);
    tui_restore_screen();
    return restart;
}


void ib_loop(const char *ilog_filename) {
    tui_init();

    struct ilog_browser _logfile = { };
    struct ilog_browser *s = &_logfile;
    ilog_open_read(&s->ilog, ilog_filename);

    s->info_h = 4;

    /* It will return 1 when it needs a restart. */
    while(ib_event_loop(s));
}


#endif
