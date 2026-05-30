#ifndef MOD_TUI
#define MOD_TUI
/* This implements the mod_tui interface for ncurses.

   Note that printf-style formatting is handled outside of this module.

   The api uses the x,y convention instead of y,x in ncurses.

   We use the idea that screen can resize, and that everything needs
   to be torn down and rebuilt in that case.
*/

// Started from claude template.
// https://claude.ai/chat/8839c53a-27fb-4565-84a0-1b856babd9af
#include <ncurses.h>
#include <string.h>
#include <stdio.h>
#include <signal.h>
#include "macros.h"

/* Global init.  For ncurses this installs a SIGWINCH handler. */
static volatile sig_atomic_t tui_resized = 0;
static void tui_on_winch(int sig) {
    (void)sig;
    // will be read when wgetch() returns ERR
    tui_resized = 1;
}
void tui_init(void) {
    struct sigaction sa = {0};
    sa.sa_handler = tui_on_winch;
    // no SA_RESTART so wgetch() will return ERR
    sigaction(SIGWINCH, &sa, NULL);
}
typedef WINDOW tui_window_t;


void tui_reverse_video(tui_window_t *w, int mode) {
    if (mode) { wattron(w, A_REVERSE); }
    else { wattroff(w, A_REVERSE); }
}

void tui_string_at(tui_window_t *w,
                   int x, int y,
                   int width,
                   const char *str)
{
    if (!width) {
        mvwprintw(w, y, x, "%s", str);
    }
    else {
        mvwprintw(w, y, x, "%-*s", width, str);
    }
}
void tui_clear(tui_window_t *w) {
    werase(w);
}
void tui_box(tui_window_t *w) {
    box(w,0,0);
}
void tui_init_screen(void) {
    initscr();
    cbreak();
    noecho();
    curs_set(0); // hide hardware cursor
}
void tui_restore_screen(void) {
    endwin();
}
int tui_cols(void) {
    return COLS;
}
int tui_lines(void) {
    return LINES;
}

tui_window_t *tui_new_window(int width, int height, int x, int y) {
    tui_window_t *w = newwin(height, width, y, x);
    keypad(w, TRUE);   // enable KEY_UP / KEY_F(n) etc.
    scrollok(w, TRUE); // permit hardware scrolling
    idlok(w, TRUE);
    return w;
}
void tui_del_window(tui_window_t *w) {
    delwin(w);
}
/* Update per window (internal) state. */
void tui_update_window(tui_window_t *w) {
    wnoutrefresh(w);
}
/* Refresh the whole screen */
void tui_update_screen(void) {
    doupdate();
}
void tui_scroll(tui_window_t *w, int lines) {
    wscrl(w, lines);
}

#define TUI_RESIZED -2  /* ERR is -1 */
#define TUI_ERR ERR

#define TUI_KEY_DOWN  KEY_DOWN
#define TUI_KEY_UP    KEY_UP
#define TUI_KEY_NPAGE KEY_NPAGE
#define TUI_KEY_PPAGE KEY_PPAGE
#define TUI_KEY_HOME  KEY_HOME
#define TUI_KEY_END   KEY_END
#define TUI_KEY_F(n)  KEY_F(n)



/* Note that in most applications the w argument can probably be ignored. */
int tui_get_key(tui_window_t *w) {
    int ch = wgetch(w);
    if (ch == ERR) {
        if (tui_resized) {
            tui_resized = 0;
            return TUI_RESIZED;
        }
        else {
            return TUI_ERR;
        }
    }
    else return ch;
}

#endif
