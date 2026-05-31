#ifndef MOD_EMSCRIPTEN_CONSOLE
#define MOD_EMSCRIPTEN_CONSOLE


// https://claude.ai/chat/7c500d35-4f3d-477a-8ad6-53cb2ce07627
// https://claude.ai/chat/4b1a5989-d60c-4a4f-b6cc-4cf858f1c138

/* Towards a "canvas panel" in the browser that can talk to a
   webscoket server (= database / daq application).

   This is an effort to make a single uc_tools text + optional
   graphics library that can do:
   - ncurses
   - vga text console
   - vga framebuffer
   - i2c lcd
   - browser canvas

   The text part should do a minimal emulation of the ncurses
   functionality:
   - window scroll, clear
   - render text at location

   The html canvas part can use an off-screen canvas to contain the
   font atlas.

*/


/*

<!DOCTYPE html>
<html>
<body>
  <canvas id="screen" width="720" height="400"></canvas>
  <script src="app.js"></script>  <!-- your emscripten output -->
</body>
</html>

*/




// Using the emscripten websocket library, this exposes the creation
// of the websocket connection and the callbacks to the C end, so no
// JS glue is needed.
#include <emscripten/emscripten.h>
#include <emscripten/websocket.h>
#include <string.h>
#include <stdio.h>

#include "leb128s.h"
#include "log_tools.h"
#include "tag_u32.c"
#include "tag_u32.h"
#include "tui_cmd.h"

void abort_busyloop(void) {
    /* Where is this coming from? */
    LOG("FIXME: abort_busyloop()\n");
    exit(1);
}

EM_JS(void, canvas_init, (int w, int h), {
    var canvas = document.getElementById("screen");
    canvas.width = w;
    canvas.height = h;
    Module.ctx = canvas.getContext("2d");
    Module.ctx.imageSmoothingEnabled = false;
});


EM_JS(void, canvas_init_font, (const uint8_t *font, int glyph_w, int glyph_h), {
    var GW = glyph_w;
    var GH = glyph_h;
    var N = 256;
    var palette = [[0,0,0],[0,0,170],[0,170,0],[0,170,170],
                   [170,0,0],[170,0,170],[170,85,0],[170,170,170],
                   [85,85,85],[85,85,255],[85,255,85],[85,255,255],
                   [255,85,85],[255,85,255],[255,255,85],[255,255,255]];

    // Build one atlas per foreground color: 16 wide grid of glyphs:
    // white on transparent.  We don't do this for background (that is
    // just fillrect).
    Module.atlas = [];
    for (var col = 0; col < 16; col++) {
        var cv = document.createElement("canvas");
        cv.width  = GW * 16;
        cv.height = GH * 16;
        var ictx = cv.getContext("2d");
        var img = ictx.createImageData(cv.width, cv.height);
        var d = img.data;
        var r = palette[col][0], g = palette[col][1], b = palette[col][2];

        for (var ch = 0; ch < N; ch++) {
            var gx = (ch & 15) * GW, gy = (ch >> 4) * GH;
            for (var row = 0; row < GH; row++) {
                var bits = HEAPU8[font + ch*GH + row];   // read C memory directly
                for (var x = 0; x < GW; x++) {
                    if (bits & (0x80 >> x)) {
                        var px = ((gy+row) * cv.width + (gx+x)) * 4;
                        d[px + 0] = r;
                        d[px + 1] = g;
                        d[px + 2] = b;
                        d[px + 3] = 255;
                    }
                }
            }
        }
        ictx.putImageData(img, 0, 0);
        Module.atlas[col] = cv;
    }
    Module.GW = GW;
    Module.GH = GH;
    Module.ctx = document.getElementById("screen").getContext("2d");
    Module.ctx.imageSmoothingEnabled = false;
});

EM_JS(void, canvas_put, (int x, int y, int code, int fg, int bg), {
    var c  = Module.ctx;
    var GW = Module.GW;
    var GH = Module.GH;
    var bgp = [[0,0,0],[0,0,170],[0,170,0],[0,170,170],
               [170,0,0],[170,0,170],[170,85,0],[170,170,170]][bg & 7];
    c.fillStyle = 'rgb(' + bgp[0] + ',' + bgp[1] + ',' + bgp[2] + ')';
    c.fillRect(x*GW, y*GH, GW, GH);
    var sx = (code & 15) * GW, sy = (code >> 4) * GH;
    c.drawImage(Module.atlas[fg & 15], sx, sy, GW, GH, x*GW, y*GH, GW, GH);
});



#if 0
// Draw one glyph cell. Here using fillRect bg + fillText fg for simplicity;
// swap fillText for your atlas blit later.
EM_JS(void, canvas_put, (int x, int y, int cw, int ch, int code, int fg, int bg), {
    var c = Module.ctx;
    var palette = ["#000","#00a","#0a0","#0aa","#a00","#a0a","#a50","#aaa",
                   "#555","#55f","#5f5","#5ff","#f55","#f5f","#ff5","#fff"];
    c.fillStyle = palette[bg];
    c.fillRect(x*cw, y*ch, cw, ch);
    c.fillStyle = palette[fg];
    c.font = ch + "px monospace";
    c.textBaseline = "top";
    c.fillText(String.fromCharCode(code), x*cw, y*ch);
});
#endif


// Self-blit scroll: copy region up by n rows, no per-cell redraw.
EM_JS(void, canvas_scroll, (int top, int bottom, int n, int cw, int ch), {
    var c = Module.ctx, cv = c.canvas;
    var y0 = top*ch, h = (bottom-top+1-n)*ch, dy = n*ch;
    c.drawImage(cv, 0, y0+dy, cv.width, h, 0, y0, cv.width, h);
});

#include "uct_byteswap.h"

typedef void (*ws_fn)(EMSCRIPTEN_WEBSOCKET_T s, void *ctx);
ws_fn  g_ws_on_open;
void  *g_ws_on_open_ctx;

struct tui_window *window[TUI_MAX_NB_WINDOWS] = {};

EM_BOOL on_open(int t, const EmscriptenWebSocketOpenEvent *e, void *u) {
    printf("connection open, sending init\n");
    g_ws_on_open(e->socket, g_ws_on_open_ctx);

    return EM_TRUE;
}

/* Handle draw commands. */
int draw(struct tag_u32 *req) {
    TAG_U32_MATCH(req, TUI_CMD_STRING_AT, m, wid, x, y, width) {
        ASSERT(m->wid < TUI_MAX_NB_WINDOWS);
        ASSERT(window[m->wid]);
        uint32_t dx = window[m->wid]->x;
        uint32_t dy = window[m->wid]->y;

        // LOG("string_at: %d %d %d %d\n", m->w, m->x, m->y, m->width);
        for (uint32_t i=0; i<req->nb_bytes; i++) {
            // FIXME: clip!
            canvas_put(m->x + dx + i,
                       m->y + dy,
                       req->bytes[i],
                       7, 0);
        }
        return 0;
    }
    TAG_U32_MATCH(req, TUI_CMD_REVERSE_VIDEO, m, wid, mode) {
        LOG("reverse_video: %d %d\n", m->wid, m->mode);
        return 0;
    }
    TAG_U32_MATCH(req, TUI_CMD_CLEAR, m, wid) {
        // LOG("clear: %d\n", m->wid);
        ASSERT(m->wid < TUI_MAX_NB_WINDOWS);
        struct tui_window *w = window[m->wid];
        ASSERT(w);
        for (uint32_t c = 0; c < w->w; c++) {
        for (uint32_t r = 0; r < w->h; r++) {
            canvas_put(w->x + c,
                       w->y + r,
                       ' ', 7, 0);
        }
        }
        return 0;
    }
    TAG_U32_MATCH(req, TUI_CMD_BOX, m, wid) {
        LOG("box: %d\n", m->wid);
        return 0;
    }
    TAG_U32_MATCH(req, TUI_CMD_INIT_SCREEN, m, cols, lines) {
        // LOG("init_screen: %d %d\n", m->cols, m->lines);
        canvas_init(8 * m->cols, 16 * m->lines);
        return 0;
    }
    TAG_U32_MATCH(req, TUI_CMD_NEW_WINDOW, m, wid, w, h, x, y) {
        LOG("new_window: %d %d %d %d %d\n", m->w, m->h, m->x, m->y, m->wid);
        struct tui_window win = {
            .id = m->wid,
            .w  = m->w,
            .h  = m->h,
            .x  = m->x,
            .y  = m->y,
        };
        ASSERT(m->wid < TUI_MAX_NB_WINDOWS);
        ASSERT(!window[m->wid]);
        window[m->wid] = malloc(sizeof(win));
        (*window[m->wid]) = win;
        return 0;
    }
    TAG_U32_MATCH(req, TUI_CMD_DEL_WINDOW, m, wid) {
        LOG("new_window: %d\n", m->wid);
        ASSERT(m->wid < TUI_MAX_NB_WINDOWS);
        ASSERT(window[m->wid]);
        free(window[m->wid]);
        window[m->wid] = NULL;
        return 0;
    }
    TAG_U32_MATCH(req, TUI_CMD_SCROLL_WINDOW, m, wid, lines) {
        LOG("scroll_windows: %d %d\n", m->wid, m->lines);
        return 0;
    }
    log_req("draw: ", req);
    return 0;
}
#define DEF_MAP DEF_TAG_U32_CONST_MAP_HANDLE

DEF_MAP(
    map_root,
    /* 0 */ {"draw", "cmd", draw},
    )

leb128_id_t push_tag_u32(struct leb128s *s, struct tag_u32 *msg) {
    if (0) {
        LOG("push_tag_u32:\n");
        log_u32("from: ", msg->from,  msg->nb_from);
        log_u32("to:   ", msg->args,  msg->nb_args);
        log_hex("bin:  ", msg->bytes, msg->nb_bytes);
    }
    int rv = map_root(msg);
    if (rv) {
        /* Always send a reply when there is a from address. */
        LOG("map_root() returned %d\n", rv);
        send_reply_tag_u32_status_cstring(msg, 1, "bad_ref");
    }
    return 0;
}


EM_BOOL on_message(int t, const EmscriptenWebSocketMessageEvent *e, void *u) {
    if (e->isText) {
        LOG("WARNING: on_message: text not supported: %s\n", e->data);
    }
    else {
        if (0) {
            LOG("on_message: %d\n", e->numBytes);
            for (int i=0; i<e->numBytes; i++) {
                LOG(" %d", e->data[i]);
            }
            LOG("\n");
        }

        /* The message is eb128s data that needs to be decoded into
           tag_u32 and routed to the handlers.  This is the same as
           websocket_push in mod_websocket_leb128s.c at the server
           end. */
        struct leb128s_env env = {
            .tag_u32 = push_tag_u32,
            .ctx = NULL,
        };
        struct leb128s s = { .buf = e->data, .len = e->numBytes, .env = &env };

        /* This calls the push_tag_u32 callback when a T_TAG message
           is received. */
        leb128_id_t id = leb128s_element(&s);
        (void)id;
        if(s.error) {
            LOG("on_message: leb128s_element error %d\n", (int)(uintptr_t)s.error);
        }
    }
    return EM_TRUE;
}

int emscripten_console_init(const char *ws_url, ws_fn ws_on_open, void *ctx) {
    g_ws_on_open     = ws_on_open;
    g_ws_on_open_ctx = ctx;
    if (!emscripten_websocket_is_supported()) {
        printf("websockets not supported\n");
        return 1;
    }
    EmscriptenWebSocketCreateAttributes attr;
    emscripten_websocket_init_create_attributes(&attr);
    attr.url = ws_url;

    EMSCRIPTEN_WEBSOCKET_T sock = emscripten_websocket_new(&attr);
    emscripten_websocket_set_onopen_callback(sock, NULL, on_open);
    emscripten_websocket_set_onmessage_callback(sock, NULL, on_message);
    return 0;  // runtime stays alive for callbacks (default NO_EXIT_RUNTIME)
}

#endif
