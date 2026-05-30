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



EM_JS(void, canvas_init, (int w, int h), {
    Module.ctx = document.getElementById("screen").getContext("2d");
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


EM_BOOL on_open(int t, const EmscriptenWebSocketOpenEvent *e, void *u) {
    printf("connection open\n");
    return EM_TRUE;
}

EM_BOOL on_message(int t, const EmscriptenWebSocketMessageEvent *e, void *u) {
    if (e->isText) {
        // text payloads are null-terminated by Emscripten, so %s is safe
        printf("received: %s\n", e->data);
        if (strcmp((const char *)e->data, "ping") == 0) {
            emscripten_websocket_send_utf8_text(e->socket, "pong");
            printf("sent: pong\n");
        }
    }
    return EM_TRUE;
}

int emscripten_console_init() {
    if (!emscripten_websocket_is_supported()) {
        printf("websockets not supported\n");
        return 1;
    }
    EmscriptenWebSocketCreateAttributes attr;
    emscripten_websocket_init_create_attributes(&attr);
    attr.url = "ws://localhost:8765";

    EMSCRIPTEN_WEBSOCKET_T sock = emscripten_websocket_new(&attr);
    emscripten_websocket_set_onopen_callback(sock, NULL, on_open);
    emscripten_websocket_set_onmessage_callback(sock, NULL, on_message);
    return 0;  // runtime stays alive for callbacks (default NO_EXIT_RUNTIME)
}

#endif
