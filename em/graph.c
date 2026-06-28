#include "mod_uct_gl.c"
#include <emscripten.h>

struct uct_gl_app _s, *s=&_s;
void frame(void) {
    uct_gl_tick(s);
};

int main(void) {
    uct_gl_open(s);
    /* 0 fps = use requestAnimationFrame; 1 = don't unwind the stack. */
    emscripten_set_main_loop(frame, 0, 1);
    return 0;
}
