/* Bridge tui C api to tag_u32 protocol, client side.

   provides: handle_tag_u32()     to handle tag_u32 messages
   uses:     req->repl(req, rpl)  to send tag_u32 replies

   See mod_tui_canvas.c */

#ifndef MOD_TUI_CLIENT
#define MOD_TUI_CLIENT

#include "tui_cmd.h"

#include "tag_u32.c"
#include "tag_u32.h"
#define DEF_MAP DEF_TAG_U32_CONST_MAP_HANDLE

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

typedef int (*tui_handle_fn)(void *, int ch);

/* To avoid back&forth we do the window id allocation at this end. */
/* Buffer allocation stack type. */
typedef struct {
    uint8_t top;
    uint8_t stack[TUI_MAX_NB_WINDOWS];
} win_free_stack_t;
typedef uint8_t win_free_element_t;
#define NS(s) win_free##s
#include "ns_stack.h"
#undef NS


/* Global variables. */
tui_handle_fn    g_handle;
void            *g_handle_ctx;
int              g_tui_cols = 80;
int              g_tui_lines = 25;
win_free_stack_t g_win_free;
void win_free_provision(void) {
    win_free_init(&g_win_free);
    for (int i=TUI_MAX_NB_WINDOWS-1; i>=0; i--) {
        win_free_push(&g_win_free, i);
    }
}


/* Events are always initiated at the browser end, e.g. it sends a key
   event as a tag_u32 request.  All drawing code in this module runs
   in the context of such a request. */
struct tag_u32 *g_req;

int key(struct tag_u32 *req) {
    // log_tag_u32("key: ", req);
    g_req = req;
    TAG_U32_UNPACK(req, 0, m, key_id) {
        LOG("key: %u\n", m->key_id);
        if (m->key_id) {
            g_handle(g_handle_ctx, m->key_id);
        }
        return 0;
    }
    return -1;
}


int init(struct tag_u32 *req) {
    if ((req->nb_args > 0) || (req->nb_bytes > 0)) {
        /* Best to do verbose logging if there is anything out of the ordinary. */
        log_tag_u32("WARNING: tui_canvas_init: ", req);
    }
    ASSERT(g_handle);
    g_handle(g_handle_ctx, TUI_BEGIN);
    return 0;
}


DEF_MAP(
    map_root,
    /* 0 */ {"init", "cmd", init},
    /* 1 */ {"key",  "cmd", key},
    )

int handle_tag_u32(struct tag_u32 *req) {
    g_req = req;
    if (0) {
        log_tag_u32("tui_client:", req);
    }
    int rv = map_root(req);
    if (rv) {
        /* Always send a reply when there is a from address. */
        LOG("map_root() returned %d\n", rv);
        send_reply_tag_u32_status_cstring(req, 1, "bad_ref");
    }
    return 0;
}

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
    //LOG("tui_string_at %d %d %s\n", x, y, str);
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
    w->id = win_free_pop(&g_win_free);
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
    win_free_push(&g_win_free, w->id);
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

void tui_init(void) {
    win_free_provision();
}

#endif
