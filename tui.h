#ifndef TUI_H
#define TUI_H

/* Window ID mapping is a fixed size array. */
#define TUI_MAX_NB_WINDOWS 64

/* Command tags for the tag_u32 protocol. */
#define TUI_CMD_STRING_AT     1
#define TUI_CMD_REVERSE_VIDEO 2
#define TUI_CMD_CLEAR         3
#define TUI_CMD_BOX           4
#define TUI_CMD_INIT_SCREEN   5
#define TUI_CMD_NEW_WINDOW    6
#define TUI_CMD_DEL_WINDOW    7
#define TUI_CMD_SCROLL_WINDOW 8
#define TUI_CMD_UPDATE_SCREEN 9

/* This is understood at the browser and server end. */
struct tui_window {
    uint32_t id;
    uint32_t w, h, x, y;
    uint32_t fg, bg;
    uint32_t reverse_video:1;
};
typedef struct tui_window tui_window_t;

#define TUI_DEFAULT_FG 7
#define TUI_DEFAULT_BG 0

/* In the client/server model both sides need to agree on these tags.
   Since the codes are arbitrary, these are taken from ncurses, so
   there is no extra translation layer necessary to glue ncurses to
   client/server. */

#define TUI_KEY_DOWN  0402  /* KEY_DOWN  */
#define TUI_KEY_UP    0403  /* KEY_UP    */
#define TUI_KEY_NPAGE 0522  /* KEY_NPAGE */
#define TUI_KEY_PPAGE 0523  /* KEY_PPAGE */
#define TUI_KEY_HOME  0406  /* KEY_HOME  */
#define TUI_KEY_END   0550  /* KEY_END   */

#define TUI_KEY_F(n) (0410 + (n))

#define TUI_ERR       -1
#define TUI_RESIZED   -2  /* ERR is -1 */
#define TUI_BEGIN     -3
#define TUI_END       -4


void tui_reverse_video(tui_window_t *w, int mode);
void tui_string_at(tui_window_t *w,
                   int x, int y,
                   int width,
                   const char *str);
void tui_clear(tui_window_t *w);
void tui_box(tui_window_t *w);
void tui_init_screen(void);
void tui_restore_screen(void);
int tui_cols(void);
int tui_lines(void);
tui_window_t *tui_new_window(int width, int height, int x, int y);
void tui_main_window(tui_window_t *w);
void tui_del_window(tui_window_t *w);
void tui_update_screen(void);
void tui_scroll(tui_window_t *w, int lines);
int tui_get_event(tui_window_t *w);
typedef int (*tui_handle_fn)(void *, int ch);

void tui_event_loop(tui_handle_fn handle, void *ctx);

#endif
