// Started from claude template.
// https://claude.ai/chat/8839c53a-27fb-4565-84a0-1b856babd9af
#include <ncurses.h>
#include <string.h>
#include <stdio.h>

#define N 100

int main(void) {
    char items[N][32];
    for (int i = 0; i < N; i++)
        snprintf(items[i], sizeof items[i], "Packet %03d", i);

    initscr();
    cbreak();
    noecho();
    curs_set(0);                 /* hide hardware cursor */

    /* Split the screen: list on top, info pane (3 rows) at bottom. */
    int info_h = 3;
    int list_h = LINES - info_h;
    WINDOW *list = newwin(list_h, COLS, 0, 0);
    WINDOW *info = newwin(info_h, COLS, list_h, 0);

    keypad(list, TRUE);          /* enable KEY_UP / KEY_F(n) etc. */
    scrollok(list, TRUE);        /* permit hardware scrolling */
    idlok(list, TRUE);

    int rows  = list_h;          /* visible list rows */
    int sel   = 0;               /* selected item index */
    int top   = 0;               /* index of item on the first visible row */

    /* draw_row: render one item at its on-screen row, highlighted or not. */
    #define draw_row(idx, highlight) do {                          \
        int y = (idx) - top;                                       \
        if ((highlight)) wattron(list, A_REVERSE);                 \
        mvwprintw(list, y, 0, "%-*s", COLS - 1, items[idx]);       \
        if ((highlight)) wattroff(list, A_REVERSE);                \
    } while (0)

    /* full list repaint — used on startup and after a page jump/scroll */
    void redraw_list(void) {                 /* (GCC nested fn; or inline it) */
        werase(list);
        for (int i = top; i < top + rows && i < N; i++)
            draw_row(i, i == sel);
    }

    /* info pane: just echo the current selection as dummy detail */
    void redraw_info(void) {
        werase(info);
        box(info, 0, 0);
        mvwprintw(info, 1, 2, "Selected: %s  (index %d)", items[sel], sel);
    }

    redraw_list();
    redraw_info();
    wnoutrefresh(list);
    wnoutrefresh(info);
    doupdate();

    int ch;
    while ((ch = wgetch(list)) != 'q') {
        int old = sel;

        if (ch == KEY_DOWN && sel < N - 1)      sel++;
        else if (ch == KEY_UP && sel > 0)       sel--;
        else if (ch == KEY_NPAGE) { sel += rows; if (sel > N-1) sel = N-1; }
        else if (ch == KEY_PPAGE) { sel -= rows; if (sel < 0)   sel = 0;   }
        /* else if (ch == KEY_F(1)) switch_view(...);  <-- your views go here */
        else continue;

        /* Has the selection scrolled off the visible window? */
        if (sel < top) {
            int delta = top - sel;            /* number of rows to scroll back */
            top = sel;
            if (delta == 1) {                 /* single step: hardware scroll */
                wscrl(list, -1);
                draw_row(sel, 1);             /* paint the row that scrolled in */
                draw_row(old, 0);             /* un-highlight old (if visible) */
            } else redraw_list();             /* page jump: just repaint */
        }
        else if (sel >= top + rows) {
            int delta = sel - (top + rows - 1);
            if (delta == 1) {
                draw_row(old, 0);        /* un-highlight while top is still old value */
                top = sel - rows + 1;
                wscrl(list, 1);
                draw_row(sel, 1);        /* new bottom row */
            } else {
                top = sel - rows + 1;
                redraw_list();
            }
        }
        else {
            /* still on screen: only the two changed rows need redrawing */
            draw_row(old, 0);
            draw_row(sel, 1);
        }

        redraw_info();
        wnoutrefresh(list);
        wnoutrefresh(info);
        doupdate();                           /* one flush, no flicker */
    }

    delwin(list);
    delwin(info);
    endwin();
    return 0;
}
