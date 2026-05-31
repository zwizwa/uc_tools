#ifndef TUI_CMD_H
#define TUI_CMD_H

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

#endif
