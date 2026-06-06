#ifndef MOD_TUI_CANVAS
#define MOD_TUI_CANVAS

#ifndef LOG
/* This goes to the javascript debug console and is line-buffered. */
#define LOG printf
#endif


/* TODO
   queue up the drawing commands and flush them in a requestAnimationFrame callback
   https://claude.ai/chat/e97f4560-47ab-414e-98d0-4717f90462da

   EDIT: A better way to do this:

   Avoid the wasm->javascript calls.  Currently it is doing a context
   switch for each character draw.  It seems better to render to a
   framebuffer in C, then blit the whole screen to the canvas on
   tui_update_screen().  Javascript->wasm calls are cheap, so pushing
   the websocket data into the wasm code should be cheap.  It can stay
   in wasm to do the rendering into an off-screen buffer, then blit
   onto the canvas in one go.

*/


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
#include "tui_vga.h"

#include "mod_vga_font_8x16.c"



static inline uint32_t tui_vga_offset_rc(struct tui_vga *log,
                                         uint32_t row,
                                         uint32_t col) {
    return 2 * (log->nb_cols * row + col);
}

int g_use_tui_vga = 1;

struct tui_vga g_tui_vga;
EMSCRIPTEN_WEBSOCKET_T g_sock;
#define tui_put canvas_put

void abort_busyloop(void) {
    /* Where is this coming from? */
    LOG("FIXME: abort_busyloop()\n");
    exit(1);
}

EM_JS(void, key_set, (const char* name, int code), {
    Module.key_ids = Module.key_ids || {};
    Module.key_ids[UTF8ToString(name)] = code;
});

void init_keys(void) {
    key_set("ArrowUp",    TUI_KEY_UP);
    key_set("ArrowDown",  TUI_KEY_DOWN);
    //key_set("ArrowLeft",  TUI_KEY_LEFT);
    //key_set("ArrowRight", TUI_KEY_RIGHT);
    key_set("PageUp",     TUI_KEY_PPAGE);
    key_set("PageDown",   TUI_KEY_NPAGE);
    key_set("Home",       TUI_KEY_HOME);
    key_set("End",        TUI_KEY_END);
    //key_set("Enter",      TUI_KEY_ENTER);
    //key_set("Backspace",  TUI_KEY_BKSP);
}

EM_JS(void, canvas_init_js, (int glyph_w, int glyph_h, uint32_t *win), {

    // Get the current vieport dimensions
    const w = window.innerWidth;
    const h = window.innerHeight;
    // console.log(w,h);

    // Convert to character dimensions.
    var c_w = Math.floor(w / glyph_w);
    var c_h = Math.floor(h / glyph_h);

    // Don't allow the window to get too small.  Better that browser displays scroll bars.
    const min_w = 20;
    const min_h = 6;
    if (c_w < min_w) c_w = min_w;
    if (c_h < min_h) c_h = min_h;

    // All return paths need to fill the current text window dimensions.
    HEAPU32[(win >> 2) + 0] = c_w;
    HEAPU32[(win >> 2) + 1] = c_h;

    // Don't resize canvas if text size did not change.
    if (Module.c_w) {
        // This is a resize.
        if ((c_w == Module.c_w) && (c_h == Module.c_h)) {
            return;
        }
    }

    // Save for next resize
    Module.c_w = c_w;
    Module.c_h = c_h;

    // Size the canvas to the available space.
    var canvas = document.getElementById("screen");
    canvas.width  = glyph_w * c_w;
    canvas.height = glyph_h * c_h;
    Module.ctx = canvas.getContext("2d");
    Module.ctx.imageSmoothingEnabled = false;
    Module.canvas = canvas;

});

void canvas_init(int glyph_w, int glyph_h, uint32_t *win) {
    canvas_init_js(glyph_w, glyph_h, win);
    if (g_tui_vga.video) free(g_tui_vga.video);
    uint32_t w = win[0];
    uint32_t h = win[1];
    typeof (g_tui_vga) *c = &g_tui_vga;
    c->video = malloc(w * h * 2);
    c->nb_cols = w;
    c->nb_rows = h;
    for (uint32_t i=0; i<w*h; i++) {
        uint32_t o = tui_vga_offset_rc(c,w,h);
        c->video[o]   = ' ';
        c->video[o+1] = 7;
    }
}



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

EM_JS(void, canvas_put_js, (int x, int y, int code, int fg, int bg), {
    var c  = Module.ctx;
    var GW = Module.GW;
    var GH = Module.GH;
    var bgp = [[0,0,0],[0,0,170],[0,170,0],[0,170,170],
               [170,0,0],[170,0,170],[170,85,0],[170,170,170]][bg & 7];
    c.fillStyle = 'rgb(' + bgp[0] + ',' + bgp[1] + ',' + bgp[2] + ')';
    c.fillRect(x*GW, y*GH, GW, GH);
    var sx = (code & 0xF) * GW, sy = (code >> 4) * GH;
    c.drawImage(Module.atlas[fg & 0xF], sx, sy, GW, GH, x*GW, y*GH, GW, GH);
});

// Blit text framebuffer to canvas.
EM_JS(void, canvas_update_screen_js, (uint8_t *framebuffer), {
    var c  = Module.ctx;
    var GW = Module.GW;
    var GH = Module.GH;
    var background = [[0,0,0],[0,0,170],[0,170,0],[0,170,170],
                      [170,0,0],[170,0,170],[170,85,0],[170,170,170]];
    for(var y=0; y<Module.c_h; y++) {
        for(var x=0; x<Module.c_w; x++) {
            var offset = framebuffer + 2 * (Module.c_w * y + x);
            var code   = HEAP8[offset];
            var attrib = HEAP8[offset+1];
            // FIXME: use double buffering and only write the updates
            var fg = attrib & 0xF;
            var bg = attrib >> 4;
            var bgp = background[bg&7];
            c.fillStyle = 'rgb(' + bgp[0] + ',' + bgp[1] + ',' + bgp[2] + ')';
            c.fillRect(x*GW, y*GH, GW, GH);
            var sx = (code & 0xF) * GW, sy = (code >> 4) * GH;
            c.drawImage(Module.atlas[fg & 0xF], sx, sy, GW, GH, x*GW, y*GH, GW, GH);
        }
    }
});

//EM_JS(void, request_canvas_update_js, (uint8_t *framebuffer), {
//    if (!Module.scheduled) {
//    }
//}


void tui_update_screen(void) {
    if (g_use_tui_vga) {
        canvas_update_screen_js(g_tui_vga.video);
    }
}



void canvas_put(int x, int y, int code, int fg, int bg) {
    if (g_use_tui_vga) {
        tui_vga_put(&g_tui_vga,
                    x, y,
                    code, fg, bg);
    }
    else {
        // Write it directly into the canvas.
        canvas_put_js(x, y, code, fg, bg);
    }
}





EM_JS(void, canvas_scroll_js, (int x, int y, int w, int h, int lines), {
    // x,y top left of window, w,h window dims, scroll lines >0 down <0 up
    var c  = Module.ctx;
    var GW = Module.GW;
    var GH = Module.GH;
    // convert from character coordiantes to pixel coordinates
    x *= GW;
    y *= GH;
    w *= GW;
    h *= GH;
    lines *= GH;

    // scroll up
    if (lines > 0) {
        var up = lines;
        c.drawImage(Module.canvas,
                    x, y+up, // source loc
                    w, h-up, // source dims
                    x, y,    // destination loc
                    w, h-up  // destination dims
            );
    }
    else {
        var dn = -lines;
        c.drawImage(Module.canvas,
                    x, y,    // source loc
                    w, h-dn, // source dims
                    x, y+dn, // destination loc
                    w, h-dn  // destination dims
            );
    }
});
void tui_scroll(tui_window_t *win, int lines) {
    if (g_use_tui_vga) {
        tui_vga_scroll(&g_tui_vga, win, lines);
    }
    else {
        canvas_scroll_js(win->x, win->y,
                         win->w, win->h,
                         lines);
    }
}



// reply/send_tag_u32 similar to mod_websocket_leb128s.c
#define WEBSOCKET_MSG_BUF 1024 // FIXME
void reply_tag_u32(const struct tag_u32 *req, const struct tag_u32 *rpl) {
    uint8_t buf[WEBSOCKET_MSG_BUF];
    struct leb128s s = {
        .buf = buf,
        .len = sizeof(buf)
    };
    leb128s_write_i32(&s, T_TAG);              if(s.error) goto error;
    leb128s_write_tag_u32_reply(&s, req, rpl); if(s.error) goto error;
    //log_hex("enc: ", s.offset, buf);
    emscripten_websocket_send_binary(g_sock, s.buf, s.len);
    return;
  error:
    LOG("leb128 write error %x\n", (unsigned int)s.error);
    return;
}
void send_tag_u32_(const struct tag_u32 *msg) {
    uint8_t buf[WEBSOCKET_MSG_BUF];
    struct leb128s s = {
        .buf = buf,
        .len = sizeof(buf)
    };
    leb128s_write_i32(&s, T_TAG);   if(s.error) goto error;
    leb128s_write_tag_u32(&s, msg); if(s.error) goto error;
    //log_hex("enc: ", s.offset, buf);
    emscripten_websocket_send_binary(g_sock, s.buf, s.len);
    return;
  error:
    LOG("leb128 write error %x\n", (unsigned int)s.error);
    return;
}
void send_tag_u32(const struct tag_u32 *msg) {
    if (msg->nb_from == 0) {
        /* Other side is not allowed to reply if from is empty.  I do
           not want to change this constraint which probably is
           load-baring in some old code.  The real solution is to fill
           this in at place where the messages originates.  I just
           want to get it to work.  FIXME remove this workaround. */
        struct tag_u32 new_msg = *msg;
        const uint32_t from[] = {0};
        new_msg.from = from;
        new_msg.nb_from = ARRAY_SIZE(from);
        send_tag_u32_(&new_msg);
    }
    else {
        send_tag_u32_(msg);
    }
}


EMSCRIPTEN_KEEPALIVE
void on_key(int tui_key_code) {
    // LOG("on_key %d\n", tui_key_code);
    SEND_TAG_U32(1 /*key*/, tui_key_code);
}
EMSCRIPTEN_KEEPALIVE
void on_resize(int w, int h) {
    // LOG("on_resize %d %d\n", w, h);
    uint32_t dims[2] = {};
    canvas_init(8,16,dims);
    SEND_TAG_U32(2 /*resized*/, dims[0], dims[1]);
}

EM_JS(void, register_events, (void), {
    const onKey    = Module.cwrap("on_key",    null, ["number"]);
    const onResize = Module.cwrap("on_resize", null, ["number", "number"]);
    window.addEventListener("keydown", (e) => {
            var tui_key_code = Module.key_ids[e.key];
            // console.log(e.key, tui_key_code);
            if (tui_key_code != undefined) {
                onKey(tui_key_code);
            }
        });
    window.addEventListener("resize",  () => {
            onResize(window.innerWidth, window.innerHeight);
        });
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




#define tui_put canvas_put
void tui_init_screen(int cols, int lines) {
    /* Note that we don't really want the application to choose the
       window size, so for now this just ignores the dimensions and
       lets canvas_init() decide. */
    uint32_t dims[2];
    canvas_init(8, 16, dims);
}


#include "mod_tui_framebuffer.c"
/* mod_tui_server defines a tag_u32 tui server in terms of local tui C api */
#include "mod_tui_server.c"


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

int tui_canvas_ws_init(const char *ws_url, ws_fn ws_on_open, void *ctx) {
    g_ws_on_open     = ws_on_open;
    g_ws_on_open_ctx = ctx;
    if (!emscripten_websocket_is_supported()) {
        printf("websockets not supported\n");
        return 1;
    }
    EmscriptenWebSocketCreateAttributes attr;
    emscripten_websocket_init_create_attributes(&attr);
    attr.url = ws_url;

    g_sock = emscripten_websocket_new(&attr);
    emscripten_websocket_set_onopen_callback(g_sock, NULL, on_open);
    emscripten_websocket_set_onmessage_callback(g_sock, NULL, on_message);


    return 0;  // runtime stays alive for callbacks (default NO_EXIT_RUNTIME)
}


uint32_t dims[2];
void app_on_open(EMSCRIPTEN_WEBSOCKET_T s, void *arg) {

    /* Once the websocket is open, we send an init message to
       mod_tui_client.c */

    LOG("send dims: %d x %d\n", dims[0], dims[1]);
    SEND_TAG_U32(0 /*init*/, dims[0], dims[1]);
}

void canvas_dbg_charset(void) {
    for (int i=0; i<256; i++) {
        int x = i % 16;
        int y = i / 16;
        canvas_put(x, y, i, 7, 0);
    }
}


void tui_canvas_init(void) {
    canvas_init(8, 16, dims);
    canvas_init_font(IBM_VGA_8x16, 8, 16);
    // cancas_dbg_charset();
    LOG("tui screen size: %d x %d\n", dims[0], dims[1]);
    init_keys();
    register_events();

    // FIXME: Use same trick as ws.js to find the url
    const char *ws = "ws://carpo:3456/ws";
    tui_canvas_ws_init(ws, app_on_open, NULL);
    // runtime stays alive for callbacks (default NO_EXIT_RUNTIME)
}


#endif
