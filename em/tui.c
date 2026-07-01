#if 1

#include "mod_tui_canvas.c"
int main() {
    LOG("rdm_browser_service\n");
    tui_canvas_init();
}

#else

// WebGL drop-in replacement.
// Temporary hack to avoid name changes in test files and straightforward switching between implemenations.

#include "mod_tui_webgl.c"
int main() {
    LOG("rdm_browser_service\n");
    tui_webgl_init();
}

#endif
