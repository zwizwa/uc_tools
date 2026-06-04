#ifndef MOD_ILOG_BROWSER
#define MOD_ILOG_BROWSER

/* This should support two interfaces:
   - plain ncurses
   - the emscripten browser canvas websocket thing

   This uses the minimal mod_tui interface.
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

#define IB_HANDLE_CONTINUE 0
#define IB_HANDLE_RESIZED  1
#define IB_HANDLE_ERROR    2
#define IB_HANDLE_QUIT     3

void ib_handle_key_event(struct ilog_browser *s, int ch) {
    int old = s->sel;
    int last = ib_nb_items(s)-1;

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
        return;
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



void ib_begin(struct ilog_browser *s) {
    tui_init_screen();

    // split screen: message window on top, info window on bottom
    s->list_h = tui_lines() - s->info_h;
    s->list_w = tui_new_window(tui_cols(), s->list_h, 0, 0);
    s->info_w = tui_new_window(tui_cols(), s->info_h, 0, s->list_h);

    tui_main_window(s->list_w);  // for ncurses quirk

    s->rows  = s->list_h;      // visible list rows

    ib_redraw_list(s);
    ib_redraw_info(s);

    tui_update_window(s->list_w);
    tui_update_window(s->info_w);
    tui_update_screen();
}
void ib_end(struct ilog_browser *s) {
    tui_del_window(s->list_w);
    tui_del_window(s->info_w);
    tui_restore_screen();
}

/* The ib is made as async as possible, with a small blocking loop
   wrapper for the blocking TUI implementations like ncurses.

   The proper async sequence is:

   ib_begin(s) on startup
   ib_handle_event(s, ch) when an event arrives
   ib_end(s) on exit event

*/

int ib_handle_event(void *ctx, int ch) {
    struct ilog_browser *s = ctx;
    // LOG("ib_handle_event %d\n", ch);

    switch(ch) {
        /* TUI_BEGIN / TUI_END are handled as events because the begin
           and end sequence cannot be sent if there is no tui
           connection yet. */
    case TUI_BEGIN: ib_begin(s); return 1;
    case TUI_END:   ib_end(s);   return 1;
        /* RESIZE, ERR, and quite are special. */
    case TUI_RESIZED:
        ib_end(s);
        ib_begin(s);
        return 1;
    case TUI_ERR:
        return 0;
    case 'q':
        return 0;
    default:
        /* The rest are ordinary key commands that update the tui. */
        ib_handle_key_event(s, ch);
        return 1;
    }
}

void ib_init(struct ilog_browser *s,
             const char *ilog_filename, int info_h) {
    /* This is C application side init.  Doesn't require a tui
       connection if the tui is connection based.  The display init
       ib_begin() is only executed once events start flowing. */
    tui_init();
    ilog_open_read(&s->ilog, ilog_filename);
    s->info_h = info_h;
}

void ib_loop(const char *ilog_filename, int info_h) {
    struct ilog_browser _logfile = { };
    struct ilog_browser *s = &_logfile;
    ib_init(s, ilog_filename, info_h);
    tui_event_loop(ib_handle_event, s);
}


#endif
