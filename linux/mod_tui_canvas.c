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

/* TAG_U32 transport is provided by the modules above.

   The module below handles the C api to TAG_U32 protocol conversion,
   i.e. a terminal client that issues drawing commands.

   Note that client/server is reversed in the TCP sense: we have the
   display server (web browser) conenct to a TCP deamon.
*/

#include "mod_tui_client.c"


void tui_event_loop(tui_handle_fn handle,
                    void *ctx) {

    g_handle = handle;
    g_handle_ctx = ctx;
    uint16_t port = 3456;
    LOG("starting server on port %d\n", port);
    webserver_loop(port);
}

#endif
