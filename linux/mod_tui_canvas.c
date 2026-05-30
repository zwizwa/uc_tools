#ifndef MOD_TUI
#define MOD_TUI

/* This implements the mod_tui interface for the canvas browser based gui
   API Notes:
   - no printf-style formatting
   - x,y convention instead of y,x from ncurses
   - screens can resize (a tui_event) but this requires teardown, reinit
   Implementation notes:
   - two threads running as coroutines (inversion of control is needed)
*/

#include "macros.h"
#include <pthread.h>
#include <stdint.h>

#include "mod_webserver.c"
#include "mod_websocket_leb128s.c"

#include "sha1.c"
#include "tag_u32.c"
#include "tag_u32.h"
#define DEF_MAP DEF_TAG_U32_CONST_MAP_HANDLE

typedef int (*tui_handle_fn)(void *, int ch);

/* Global variables. */
tui_handle_fn g_handle;
int g_tui_cols = 80;
int g_tui_lines = 25;
int g_next_id = 0;

/* Events are always initiated at the browser end, e.g. it sends a key
   event as a tag_u32 request.  All drawing code in this module runs
   in the context of such a request. */
struct tag_u32 *g_req;

int key(struct tag_u32 *req) {
    g_req = req;
    TAG_U32_UNPACK(req, 0, m, key_id) {
        return 0;
    }
    return -1;
}
int init(struct tag_u32 *req) {
    g_req = req;
    TAG_U32_UNPACK(req, 0, m, key_id) {
        return 0;
    }
    return -1;
}


DEF_MAP(
    event,
    {"key", "cmd", key, 0}
    )

DEF_MAP(
    map_root,
    {"event", "map", event},
    {"init",  "cmd", init},
    )

int handle_tag_u32(struct tag_u32 *req) {
    g_req = req;
    if (0) {
        LOG("handle_tag_u32: (%d)", req->nb_args);
        for (uint32_t i=0; i<req->nb_args; i++) {
            LOG(" %d", req->args[i]);
        }
        LOG("\n");
    }
    int rv = map_root(req);
    if (rv) {
        /* Always send a reply when there is a from address. */
        LOG("map_root() returned %d\n", rv);
        send_reply_tag_u32_status_cstring(req, 1, "bad_ref");
    }
    return 0;
}

struct tui_window {
    int id;
    int w, h, x, y;
    int reverse_video:1;
};
typedef struct tui_window tui_window_t;

#define TUI_CMD_STRING_AT     1
#define TUI_CMD_REVERSE_VIDEO 2
#define TUI_CMD_CLEAR         3
#define TUI_CMD_BOX           4
#define TUI_CMD_INIT_SCREEN   5
#define TUI_CMD_NEW_WINDOW    6
#define TUI_CMD_DEL_WINDOW    7
#define TUI_CMD_SCROLL_WINDOW 8

void tui_reverse_video(tui_window_t *w, int mode) {
    SEND_REPLY_TAG_U32(
        g_req,
        TUI_CMD_REVERSE_VIDEO,
        w->id,
        mode
        );
}


void tui_string_at(tui_window_t *w,
                   int x, int y,
                   int width,
                   const char *str) {
    SEND_REPLY_TAG_U32_BYTES(
        g_req,
        ((const uint8_t*)str), strlen(str),
        TUI_CMD_STRING_AT,
        w->id,
        x, y, width
        );
}
void tui_clear(tui_window_t *w) {
    SEND_REPLY_TAG_U32(
        g_req,
        TUI_CMD_CLEAR,
        w->id);
}
void tui_box(tui_window_t *w) {
    SEND_REPLY_TAG_U32(
        g_req,
        TUI_CMD_BOX,
        w->id);
}
void tui_init_screen(void) {
    /* The w,h can be passed in on the command line and is used to
       initialize the canvas size. */
    SEND_REPLY_TAG_U32(
        g_req,
        TUI_CMD_INIT_SCREEN,
        g_tui_cols,
        g_tui_lines);
}
void tui_restore_screen(void) {
    /* nop */
}
int tui_cols(void) {
    return g_tui_cols;
}
int tui_lines(void) {
    return g_tui_lines;
}

tui_window_t *tui_new_window(int width, int height, int x, int y) {
    tui_window_t *w = malloc(sizeof(*w));
    memset(w,0,sizeof(*w));
    w->h = height;
    w->w = width;
    w->x = x;
    w->y = y;
    w->id = g_next_id++;
    SEND_REPLY_TAG_U32(
        g_req,
        TUI_CMD_NEW_WINDOW,
        w->id,
        width, height, x, y);
    return w;
}
void tui_del_window(tui_window_t *w) {
    SEND_REPLY_TAG_U32(
        g_req,
        TUI_CMD_DEL_WINDOW,
        w->id);
    free(w);
}
/* Update per window (internal) state and per screen are nop: all
   drawing commands are written directly to the canvas. */
void tui_update_window(tui_window_t *w) { }
void tui_update_screen(void) { }
void tui_main_window(tui_window_t *w) { }

void tui_scroll(tui_window_t *w, int lines) {
    SEND_REPLY_TAG_U32(
        g_req,
        TUI_CMD_SCROLL_WINDOW,
        w->id,
        lines);
}

#define TUI_ERR       -1
#define TUI_RESIZED   -2  /* ERR is -1 */
#define TUI_BEGIN     -3
#define TUI_END       -4

#define TUI_KEY_DOWN   1
#define TUI_KEY_UP     2
#define TUI_KEY_NPAGE  3
#define TUI_KEY_PPAGE  4
#define TUI_KEY_HOME   5
#define TUI_KEY_END    6
#define TUI_KEY_F(n)   (0x10 + ((n)&15))



void tui_init(void) {
}

void tui_event_loop(tui_handle_fn handle,
                    void *ctx) {

    g_handle = handle;
    uint16_t port = 3456;
    LOG("starting server on port %d\n", port);
    webserver_loop(port);
}

#endif
