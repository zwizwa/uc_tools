#define LOG printf

#include "mod_vga_font_8x16.c"
#include "mod_emscripten_console.c"

int main() {

    //canvas_init(640,480);
    canvas_init(1280,640);
    canvas_init_font(IBM_VGA_8x16, 8, 16);
    int fg = 7;
    int bg = 0;
    for (int i=0; i<256; i++) {
        int x = i % 32;
        int y = i / 32;
        canvas_put(x, y, i, fg, bg);
    }

    // emscripten_console_init();
    LOG("rdm-bridge em\n");

    return 0;  // runtime stays alive for callbacks (default NO_EXIT_RUNTIME)
}

