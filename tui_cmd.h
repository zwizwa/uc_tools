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
    int id;
    int w, h, x, y;
    int reverse_video:1;
};
typedef struct tui_window tui_window_t;

#endif
