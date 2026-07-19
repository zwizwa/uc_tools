
#ifndef MOD_TUI_VGA
#define MOD_TUI_VGA


/* I regret making tui a singleton API -- it was modeled after
   ncurses.  Provide both tui_vga_ generic methods, and tui_ methods
   bound to global g_tui_vga object. */
extern struct tui_vga *g_tui_vga;

void tui_put(
    uint32_t x, uint32_t y, // absolute cordinates
    uint32_t c,   // character to write
    uint32_t fg,  // forground color
    uint32_t bg)  // background color
{
    tui_vga_put(g_tui_vga,x,y,c,fg,bg);
}

void tui_clear(struct tui_window *win) {
    tui_vga_clear(g_tui_vga, win);
}

#endif

