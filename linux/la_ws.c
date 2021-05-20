/* Logic Analyzer Websocket interface

   This takes an 8x1 stream on stdin (e.g. what comes out of the
   Saleae Logic8), pushes it into a circular buffer, keeps track of a
   mipmap representation, and provides access to the data via a web
   interface.

   The JavaScript part is found elsewhere.
*/


/* View and model updates need to be mutually exclusive. */
#include "os_thread.h"
os_mutex_t minmax_lock;
#define MINMAX_EXCLUSIVE() \
    WITH_PRE_POST(&minmax_lock, os_mutex_lock, os_mutex_unlock)

/* Startup sequence:
   - the elf generated from this file starts first
   - it opens a TCP socket with a web server where browser can connect
   - this serves WEBSERVER_INDEX_HTML as root document
   - which starts la_ws.js to set up a web socket and basic ui. */
#define WEBSERVER_INDEX_HTML "la_ws.html"

//#define MMAP_FILE_LOG LOG
#include "mod_minmax_8x1.c"
#include "mod_minmax_viewmodel.c"
#include "mod_webserver.c"
#include "mod_websocket_leb128s.c"


OS_THREAD_STACK(http_thread, 1024);
uint16_t tcp_port = 0;
OS_THREAD_MAIN(http_loop, ctx) {
    webserver_loop(tcp_port);
    return 0;
}

/* Once slice is currently 1MB, same as Saleae C++ code buffer size.
   Samplerate is currently 2Msps
   So once slice is half a second.
   30 seconds is probably enough. */

uintptr_t nb_slices;

int la_loop(void) {
    uint8_t *circ_buf = map.level[0].buf;
    uintptr_t slice_nb= 0;
    for(;;) {
        LOG("\r%03d", slice_nb);
        uint8_t *buf = circ_buf + MINMAX_SLICE_SIZE * slice_nb;
        /* Note that we are writing straight into the buffer without
           locking.  If gui requests level 0 data it might get new or
           old. We can't just put the read in a critical section
           because then there is no time slot left for the view.
           However, only the data changes, and none of the indexing,
           so the worst that can happen is display glitches.  Deal
           with it later. */
        assert_read_fixed(0, buf, MINMAX_SLICE_SIZE);
        minmax_update_slice(&map, slice_nb);
        slice_nb = (slice_nb + 1) % nb_slices;
    }
    return 0;
}


// ws = new WebSocket("ws://10.1.3.29:3456");
// see test_websocket_timeseries.sh

int main(int argc, char **argv) {
    if (argc != 5) {
        LOG("usage: %s <documentroot> <buffer> <nb_slices> <tcp_port>\n", argv[0]);
        return 1;
    }

    nb_slices = atoi(argv[3]);
    tcp_port = atoi(argv[4]);
    minmax_open_buf(&map, argv[2], 8, nb_slices);

    ASSERT_ERRNO(chdir(argv[1]));
    OS_THREAD_START(http_thread, http_loop, NULL);

    return la_loop();
}