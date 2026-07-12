#ifndef MOD_QUERY_BROWSER
#define MOD_QUERY_BROWSER

/* Trying to find an abstract way to write simple navigation TUIs on
   top of database queries.

   - Use vtables or CSV imports to put all data in a sqlite3 database

   - Define top query + formatter

   - For each result, define how to expand top query into the
     zoomed-in view

   Example:
   - top level is list of executed tests
   - 2nd level is logs inside the tests
   - 3rd level is any expansion possible for a single (structured) log view

   C code that depends on the schema will go into the main binary.
   Code in this mod should be generic.

*/


#ifndef MOD_TUI
#include "mod_tui_ncurses.c"
#endif


/* Note that mod_sqlite.c uses a single global db pointer and a global
   cache of prepared statements. */
#include "mod_sqlite3.c"

/* Arena allocator. */
#include "mmap_bump.h"

/* The query result representation is essentially:
   - get number of records
   - get string representation of indexed record

   From the pov of the TUI we just switch tables in response to
   keyboard input.

   User code can represent however it wants.
*/




struct query_result {
    const char *display;
    void *handle;
};
struct query_array {
    struct query_result *buf;
    uintptr_t count;
    uintptr_t room;
};

struct query_browser {
    int info_h;        // hight of info window, including border
    int list_h;        // light of message list window
    tui_window_t *list_w;
    tui_window_t *info_w;
    int rows;          // number of visible list rows
    int sel;           // selected message index
    int top;           // top row message index

    /* Provide arena allocator to the renderer. */
    struct arena arena;

    /* State of the UI state machine, e.g. contains O(1)
       representation of the current list. */
    void *state;
};

/* This needs to be called in every function that uses s->table
   We call it in the render function. */
uintptr_t qb_nb_rows(struct query_browser *s);
void qb_enter(struct query_browser *s, uintptr_t index);
void qb_format(struct query_browser *s,
               uintptr_t index,
               char *buf,
               uintptr_t buf_size);


// render message on the correct row, highlighting on/off
void qb_draw_row(struct query_browser *s, int index, int highlight) {
    int y = (index) - s->top;
    char buf[tui_cols()];
    buf[0] = 0;
    qb_format(s, index, buf, sizeof(buf));
    int x = 0;
    tui_reverse_video(s->list_w, highlight);
    tui_string_at(s->list_w, x, y, tui_cols()-1, buf);
    tui_reverse_video(s->list_w, 0);
}

// redraw all messages in the message window
void qb_redraw_list(struct query_browser *s) {
    tui_clear(s->list_w);
    int n = qb_nb_rows(s);
    for (int i = s->top; i < s->top + s->rows && i < n; i++) {
        qb_draw_row(s, i, i == s->sel);
    }
}

// draw message info
void qb_redraw_info(struct query_browser *s) {
    tui_clear(s->info_w);
    tui_box(s->info_w);
    char buf[100];
    int n = qb_nb_rows(s);
    snprintf(buf, sizeof(buf), "Selected: index %d (size = %d)", s->sel, n);
    tui_string_at(s->info_w, 2, 1, 0, buf);
}

#define QB_HANDLE_CONTINUE 0
#define QB_HANDLE_RESIZED  1
#define QB_HANDLE_ERROR    2
#define QB_HANDLE_QUIT     3

void qb_handle_key_event(struct query_browser *s, int ch) {

    int old = s->sel;
    int last = qb_nb_rows(s) - 1;

    /* Regular keys. */
    if (ch == TUI_KEY_DOWN && s->sel < last) {
        s->sel++;
    }
    else if (ch == TUI_KEY_UP && s->sel > 0) {
        s->sel--;
    }
    else if (ch == TUI_KEY_RIGHT) {
        /* Are these invariants?  Code above doesn't seem to think so.  FIXME. */
        ASSERT(s->sel >= 0);
        ASSERT(s->sel <= last);
        if (s->sel < 0)    s->sel = 0;
        if (s->sel > last) s->sel = last;
        qb_enter(s, s->sel);
        qb_redraw_list(s);
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
            qb_draw_row(s, s->sel, 1);     // paint the row that scrolled in
            qb_draw_row(s, old, 0);        // un-highlight old (if visible)
        } else {
            qb_redraw_list(s);             // page jump: just redraw everything
        }
    }
    else if (s->sel >= s->top + s->rows) {
        int delta = s->sel - (s->top + s->rows - 1);
        if (delta == 1) {
            qb_draw_row(s, old, 0);        // un-highlight while top is still old value
            s->top = s->sel - s->rows + 1;
            tui_scroll(s->list_w, 1);
            qb_draw_row(s, s->sel, 1);     // new bottom row
        } else {
            s->top = s->sel - s->rows + 1;
            qb_redraw_list(s);
        }
    }
    else {
        // still on screen: only the two changed rows need redrawing
        qb_draw_row(s, old, 0);
        qb_draw_row(s, s->sel, 1);
    }

    qb_redraw_info(s);
    tui_update_window(s->list_w);
    tui_update_window(s->info_w);
    tui_update_screen();  // one flush, no flicker

}



void qb_begin(struct query_browser *s) {
    tui_init_screen();

    // split screen: message window on top, info window on bottom
    s->list_h = tui_lines() - s->info_h;
    s->list_w = tui_new_window(tui_cols(), s->list_h, 0, 0);
    s->info_w = tui_new_window(tui_cols(), s->info_h, 0, s->list_h);

    tui_main_window(s->list_w);  // for ncurses quirk

    s->rows  = s->list_h;      // visible list rows

    qb_redraw_list(s);
    qb_redraw_info(s);

    tui_update_window(s->list_w);
    tui_update_window(s->info_w);
    tui_update_screen();
}
void qb_end(struct query_browser *s) {
    tui_del_window(s->list_w);
    tui_del_window(s->info_w);
    tui_restore_screen();
}

/* The ib is made as async as possible, with a small blocking loop
   wrapper for the blocking TUI implementations like ncurses.

   The proper async sequence is:

   qb_begin(s) on startup
   qb_handle_event(s, ch) when an event arrives
   qb_end(s) on exit event

*/

int qb_handle_event(void *ctx, int ch) {
    struct query_browser *s = ctx;
    // LOG("qb_handle_event %d\n", ch);

    switch(ch) {
        /* TUI_BEGIN / TUI_END are handled as events because the begin
           and end sequence cannot be sent if there is no tui
           connection yet. */
    case TUI_BEGIN: qb_begin(s); return 1;
    case TUI_END:   qb_end(s);   return 1;
        /* RESIZE, ERR, and quite are special. */
    case TUI_RESIZED:
        qb_end(s);
        qb_begin(s);
        return 1;
    case TUI_ERR:
        return 0;
    case 'q':
        return 0;
    default:
        /* The rest are ordinary key commands that update the tui. */
        qb_handle_key_event(s, ch);
        return 1;
    }
}

void qb_init(struct query_browser *s,
             const char *db_filename,
             const char *const *extensions) {
    /* This is C application side init.  Doesn't require a tui
       connection if the tui is connection based.  The display init
       qb_begin() is only executed once events start flowing. */
    tui_init();
    db_open(db_filename);
    db_load_extensions(extensions);
    s->info_h = 4;
    arena_init(&s->arena);
}

void qb_loop(const char *db_filename, const char *const *exts) {
    struct query_browser _logfile = { };
    struct query_browser *s = &_logfile;
    qb_init(s, db_filename, exts);
    tui_event_loop(qb_handle_event, s);
}


#endif
