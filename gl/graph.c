#include "mod_uct_gl.c"

int main(void) {
    struct uct_gl_app _s, *s=&_s;
    uct_gl_open(s);
    for (s->running = 1; s->running; ) {
        uct_gl_tick(s);
    }
    uct_gl_close(s);
    return 0;
}
