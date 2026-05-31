#ifndef MOD_TUI
#define MOD_TUI

/* This implements the mod_tui interface for the canvas browser based gui
   API Notes:
   - no printf-style formatting
   - x,y convention instead of y,x from ncurses
   - screens can resize (a tui_event) but this requires teardown, reinit
   Implementation notes:
   - two threads running as coroutines (inversion of control is needed)
   - this is more general than "browser canvas" and could support other panel apps
*/

#include "macros.h"
#include <pthread.h>
#include <stdint.h>

#include "mod_webserver.c"
#include "mod_websocket_leb128s.c"
#include "sha1.c"

/* Note that the code in mod_tui_canvas is just a bridge between TUI C
   API and TAG_U32.  It is now hard-coded to websocket_leb128s.c but
   any other TAG_U32 endpoint could be driven.  This is why the core
   is separate. */
#include "mod_tui_tag_u32.c"


void tui_init(void) {
}

void tui_event_loop(tui_handle_fn handle,
                    void *ctx) {

    g_handle = handle;
    g_handle_ctx = ctx;
    uint16_t port = 3456;
    LOG("starting server on port %d\n", port);
    webserver_loop(port);
}

#endif
