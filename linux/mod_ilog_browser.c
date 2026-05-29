#ifndef MOD_ILOG_BROWSER
#define MOD_ILOG_BROWSER

// Started from claude template.
// https://claude.ai/chat/8839c53a-27fb-4565-84a0-1b856babd9af
#include <ncurses.h>
#include <string.h>
#include <stdio.h>
#include <signal.h>
#include "macros.h"
#include "ilog.h"

static volatile sig_atomic_t resized = 0;
static void ib_on_winch(int sig) {
    (void)sig;
    // will be read when wgetch() returns ERR
    resized = 1;
}

void ib_install_signal_handler(void) {
    struct sigaction sa = {0};
    sa.sa_handler = ib_on_winch;
    // no SA_RESTART so wgetch() will return ERR
    sigaction(SIGWINCH, &sa, NULL);
}

struct ilog_browser {
    int info_h;        // hight of info window, including border
    int list_h;        // light of message list window
    WINDOW *list_w;
    WINDOW *info_w;
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
    if ((highlight)) wattron(s->list_w, A_REVERSE);
    char buf[COLS];
    ib_format_message(s, index, buf, sizeof(buf));
    mvwprintw(s->list_w, y, 0, "%-*s", COLS - 1, buf);
    if ((highlight)) wattroff(s->list_w, A_REVERSE);
}

// redraw all messages in the message window
void ib_redraw_list(struct ilog_browser *s) {
    werase(s->list_w);
    for (int i = s->top; i < s->top + s->rows && i < ib_nb_items(s); i++) {
        ib_draw_row(s, i, i == s->sel);
    }
}

// draw message info
void ib_redraw_info(struct ilog_browser *s) {
    werase(s->info_w);
    box(s->info_w, 0, 0);
    mvwprintw(s->info_w, 1, 2, "Selected: index %d", s->sel);
}

// initialize screen, handle events, restore screen
// exits on 'q' or SIGWINCH
int ib_event_loop(struct ilog_browser *s) {

    int rv = 0;

    initscr();
    cbreak();
    noecho();
    curs_set(0); // hide hardware cursor

    // split screen: message window on top, info window on bottom
    s->list_h = LINES - s->info_h;
    s->list_w = newwin(s->list_h, COLS, 0, 0);
    s->info_w = newwin(s->info_h, COLS, s->list_h, 0);

    keypad(s->list_w, TRUE);   // enable KEY_UP / KEY_F(n) etc.
    scrollok(s->list_w, TRUE); // permit hardware scrolling
    idlok(s->list_w, TRUE);

    s->rows  = s->list_h;      // visible list rows

    ib_redraw_list(s);
    ib_redraw_info(s);
    wnoutrefresh(s->list_w);
    wnoutrefresh(s->info_w);
    doupdate();

    for(;;) {
        int ch = wgetch(s->list_w);
        int old = s->sel;
        int last = ib_nb_items(s)-1;

        if (ch == ERR) {
            if (resized) {
                resized = 0;
                rv = 1;
            }
            else {
                rv = 2;
            }
            break;
        }
        else if (ch == 'q') {
            rv = 0;
            break;
        }
        else if (ch == KEY_DOWN && s->sel < last) {
            s->sel++;
        }
        else if (ch == KEY_UP && s->sel > 0) {
            s->sel--;
        }
        else if (ch == KEY_NPAGE) {
            s->sel += s->rows;
            if (s->sel > last) {
                s->sel = last;
            }
        }
        else if (ch == KEY_PPAGE) {
            s->sel -= s->rows;
            if (s->sel < 0) {
                s->sel = 0;
            }
        }
        else if (ch == KEY_HOME) {
            s->sel = 0;
        }
        else if (ch == KEY_END) {
            s->sel = last;
        }

        //  else if (ch == KEY_F(1)) { handle_f1(...); }
        else {
            // other keys don't update layout
            continue;
        }

        // Has the selection scrolled off the visible window? */
        if (s->sel < s->top) {
            int delta = s->top - s->sel;       // number of rows to scroll back
            s->top = s->sel;
            if (delta == 1) {                  // single step: hardware scroll
                wscrl(s->list_w, -1);
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
                wscrl(s->list_w, 1);
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
        wnoutrefresh(s->list_w);
        wnoutrefresh(s->info_w);
        doupdate();  // one flush, no flicker
    }

    delwin(s->list_w);
    delwin(s->info_w);
    endwin();

    return rv;
}

void ib_loop(const char *ilog_filename) {
    ib_install_signal_handler();

    struct ilog_browser _logfile = { };
    struct ilog_browser *s = &_logfile;
    ilog_open_read(&s->ilog, ilog_filename);

    s->info_h = 4;

    for(;;) {
        int rv = ib_event_loop(s);
        // LOG("rv = %d, resized = %d\n", rv, resized);
        if (rv == 0) {
            /* Normal exit. */
            exit(0);
        }
    }
}


#endif
