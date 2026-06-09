#ifndef MOD_TUI_CANVAS
#define MOD_TUI_CANVAS

#ifndef LOG
/* This goes to the javascript debug console and is line-buffered. */
#define LOG printf
#endif

/* TODO

   - On resize, something gets out of sync.  I think there are still
     drawing commands in flight for a bigger window when the canvas
     size gets reduced.  Maybe canvas size should only reduce in
     response to client request.  That way it has the coorect view.
*/


/* This is a "canvas panel" in the browser that can talk to a
   websocket server (= database / daq application).

   It is built on the "mod_tui" set of files.  A single
   ncurses-inspired minimalistic tui API that can do:

   - ncurses
   - browser canvas
   - vga text console
   - vga framebuffer (TODO)
   - i2c lcd (TODO)
*/



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
#include "mod_terminus_font_8x16.c"

int g_use_tui_vga = 1;
struct tui_vga g_tui_vga;
EMSCRIPTEN_WEBSOCKET_T g_sock;
uint32_t g_max_video = 0;
const char *g_ws_url = "ws://carpo:3456/ws";


void abort_busyloop(void) {
    /* Where is this coming from? */
    LOG("FIXME: abort_busyloop()\n");
    exit(1);
}

EM_JS(void, key_set_js, (const char* name, int code), {
    Module.key_ids = Module.key_ids || {};
    Module.key_ids[UTF8ToString(name)] = code;
});

void init_keys(void) {
    key_set_js("ArrowUp",    TUI_KEY_UP);
    key_set_js("ArrowDown",  TUI_KEY_DOWN);
    //key_set_js("ArrowLeft",  TUI_KEY_LEFT);
    //key_set_js("ArrowRight", TUI_KEY_RIGHT);
    key_set_js("PageUp",     TUI_KEY_PPAGE);
    key_set_js("PageDown",   TUI_KEY_NPAGE);
    key_set_js("Home",       TUI_KEY_HOME);
    key_set_js("End",        TUI_KEY_END);
    //key_set_js("Enter",      TUI_KEY_ENTER);
    //key_set_js("Backspace",  TUI_KEY_BKSP);
}

EM_JS(void, canvas_close_js, (void), {
    const onResize = Module.cwrap("on_resize", null, ["number", "number"]);

    /* Reduce size to 0x0 to effectively make it disappear. */
    var canvas = document.getElementById("screen");
    canvas.width  = 0;
    canvas.height = 0;

    /* Reset text size as well to make sure canvas_init() actually
       resizes on reconnect. */
    Module.c_w    = 0;
    Module.c_h    = 0;

#if 0
    /* Schedule a reconnect attempt once per second.

       Note that it would probably be best to completely reload the
       page when a reconnect succeeds, to also get the updated browser
       code.  Though this is only needed during development.  It could
       be done automatically by comparing version strings. */
    setTimeout(
        Module.cwrap("tui_canvas_ws_init", null, []),
        1000);

    /* FIXME: Auto-reconnect basically works, but I've disabled this
       feature, because opening a second window will kill the first
       one, and they will go in a mutual stealing regime. */
#endif
});


EM_JS(void, canvas_init_js, (int glyph_w, int glyph_h, uint32_t *win, int apply), {

    // Get the current vieport dimensions
    const w = window.innerWidth;
    const h = window.innerHeight;
    // console.log(w,h);

    // Round down to fit a character matrix.
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

    if (!apply) {
        // Don't touch the canvas.
        return;
    }

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

    // The blink bit is not implemented.
    Module.background =
        [[  0, 0, 0], [  0,  0,170], [  0,170,  0], [  0,170,170],
         [170, 0, 0], [170,  0,170], [170, 85,  0], [170,170,170]]

});


void canvas_init(int glyph_w, int glyph_h, uint32_t *win, int apply) {
    canvas_init_js(glyph_w, glyph_h, win, apply);
    uint32_t w = win[0];
    uint32_t h = win[1];
    if (!apply) {
        /* Don't apply the dimensions to the buffer yet because
         * drawing might get out of sync. */
        return;
    }
    typeof (g_tui_vga) *c = &g_tui_vga;
    c->nb_cols = w;
    c->nb_rows = h;
    uint32_t video_size = w * h * 2;
    if (!g_tui_vga.video) {
        g_max_video = video_size;
        c->video = malloc(g_max_video);
    }
    else if (video_size <= g_max_video) {
        /* Don't shrink buffer.  This is a workaround for in-flight
           drawing commands, to make sure they don't hit outside of
           the array if the window is shrunk.  Fix this properly:
           window should resize in sync with what client thinks.
           Probably @update_screen. */
    }
    else {
        g_max_video = video_size;
        c->video = realloc(c->video, g_max_video);
    }
    for (uint32_t i=0; i<w*h; i++) {
        c->video[i*2]   = ' ';
        c->video[i*2+1] = 7;
    }
}



EM_JS(void, canvas_init_font_js, (const uint8_t *font, int glyph_w, int glyph_h), {
    var GW = glyph_w;
    var GH = glyph_h;
    var N = 256;
    var palette =
        [[  0,  0,  0], [  0,  0,170], [  0,170,  0], [  0,170,170],
         [170,  0,  0], [170,  0,170], [170, 85,  0], [170,170,170],
         [ 85, 85, 85], [ 85, 85,255], [ 85,255, 85], [ 85,255,255],
         [255, 85, 85], [255, 85,255], [255,255, 85], [255,255,255]];

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
    var bgp = Module.background[bg & 7];
    c.fillStyle = "rgb(" + bgp[0] + "," + bgp[1] + "," + bgp[2] + ")";
    c.fillRect(x*GW, y*GH, GW, GH);
    var sx = (code & 0xF) * GW, sy = (code >> 4) * GH;
    c.drawImage(Module.atlas[fg & 0xF], sx, sy, GW, GH, x*GW, y*GH, GW, GH);
});

// Blit text framebuffer to canvas when animation frame is available.
EM_JS(void, request_canvas_update_js, (uint8_t *framebuffer), {
    function canvas_update() {
        // console.log("canvas_update");
        Module.redraw_scheduled = false;
        var c  = Module.ctx;
        var GW = Module.GW;
        var GH = Module.GH;
        var background = Module.background;
        for(var y=0; y<Module.c_h; y++) {
            for(var x=0; x<Module.c_w; x++) {
                var offset = framebuffer + 2 * (Module.c_w * y + x);
                var code   = HEAPU8[offset];
                var attrib = HEAPU8[offset+1];
                // FIXME: use double buffering and only write the updates
                var fg = attrib & 0xF;
                var bg = attrib >> 4;
                var bgp = background[bg&7];
                c.fillStyle = "rgb(" + bgp[0] + "," + bgp[1] + "," + bgp[2] + ")";
                c.fillRect(x*GW, y*GH, GW, GH);
                var sx = (code & 0xF) * GW, sy = (code >> 4) * GH;
                c.drawImage(Module.atlas[fg & 0xF], sx, sy, GW, GH, x*GW, y*GH, GW, GH);
           }
        }
    }
    if (!Module.redraw_scheduled) {
        Module.redraw_scheduled = true;
        requestAnimationFrame(canvas_update);
    }
});


void tui_update_screen(void) {
    if (g_use_tui_vga) {
        request_canvas_update_js(g_tui_vga.video);
        // This is also when we should send out pending resizes.
        // E.g. always wait until rendering is done.
    }
    else {
        // updates happen synchronously
    }
}



void tui_put(uint32_t x, uint32_t y, uint32_t code, uint32_t fg, uint32_t bg) {
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

    // FIXME: When using the text frame buffer, the resize needs to
    // happen in sync with the drawing commands.  Actually any drawing
    // commands can be dropped.

    // Alternatively: never make the memory buffer smaller.  That way
    // there are no out-of-bounds accesses that can mess things up.

    uint32_t dims[2] = {};

    // Note that we can't apply the new framebuffer dimensions yet
    // 1. not necessary: client will re-initialize after resized event, and
    // 2. wrong: there might be old drawing commands in flight that assume the old layout
    canvas_init(8,16,dims, 0 /* don't apply */);

    LOG("resized: %d x %d\n", dims[0], dims[1]);
    SEND_TAG_U32(2 /*resized*/, dims[0], dims[1]);


    // ACTUALLY it is better to just discard all drawing commands
    // until the next tui_init_screen() that matches the current
    // config, because there really is no way to prevent the resize.
}


EM_JS(void, register_events_js, (void), {
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

struct tui_window *window[TUI_MAX_NB_WINDOWS] = {};
void window_cleanup(void) {
    /* Only call this when connection breaks. */
    for(int i=0; i<TUI_MAX_NB_WINDOWS; i++) {
        if (window[i]) {
            LOG("cleaning up window %d\n", i);
            free(window[i]);
            window[i] = NULL;
        }
    }
}


void tui_init_screen(int cols, int lines) {
    /* Note that we don't really want the application to choose the
       window size, so for now this just ignores the dimensions and
       lets canvas_init() decide. */
    uint32_t dims[2];
    canvas_init(8, 16, dims, 1 /* apply */);
    /* Check that we are in sync. */
    if ((cols  != dims[0]) ||
        (lines != dims[1])) {
        LOG("WARNING: client %d x %d and canvas %d x %d dimensions do not match\n",
            cols, lines, dims[0], dims[1]);
    }
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

uint32_t dims[2];

EM_BOOL on_open(int t, const EmscriptenWebSocketOpenEvent *e, void *u) {
    canvas_init(8, 16, dims, 1);
    printf("connection open, sending init\n");
    LOG("send dims: %d x %d\n", dims[0], dims[1]);
    SEND_TAG_U32(0 /*init*/, dims[0], dims[1]);
    return EM_TRUE;
}

EM_BOOL on_close(int t, const EmscriptenWebSocketCloseEvent *e, void *u) {
    printf("connection closed\n");
    canvas_close_js();
    window_cleanup();
    return EM_TRUE;
}
EMSCRIPTEN_KEEPALIVE
int tui_canvas_ws_init(void) {
    if (!emscripten_websocket_is_supported()) {
        printf("websockets not supported\n");
        return 1;
    }
    EmscriptenWebSocketCreateAttributes attr;
    emscripten_websocket_init_create_attributes(&attr);
    attr.url = g_ws_url;

    g_sock = emscripten_websocket_new(&attr);
    emscripten_websocket_set_onopen_callback(g_sock, NULL, on_open);
    emscripten_websocket_set_onmessage_callback(g_sock, NULL, on_message);
    emscripten_websocket_set_onclose_callback(g_sock, NULL, on_close);


    return 0;  // runtime stays alive for callbacks (default NO_EXIT_RUNTIME)
}



void canvas_dbg_charset(void) {
    for (int i=0; i<256; i++) {
        int x = i % 16;
        int y = i / 16;
        tui_put(x, y, i, 7, 0);
    }
}


void tui_canvas_init(void) {

    //canvas_init_font_js(IBM_VGA_8x16, 8, 16);
    //canvas_init_font_js(terminus_bold_8x16, 8, 16);
    canvas_init_font_js(terminus_vga_8x16, 8, 16);


    // cancas_dbg_charset();
    LOG("tui screen size: %d x %d\n", dims[0], dims[1]);
    init_keys();
    register_events_js();

    // FIXME: Use same trick as ws.js to find the url
    tui_canvas_ws_init();
    // runtime stays alive for callbacks (default NO_EXIT_RUNTIME)
}


#endif
