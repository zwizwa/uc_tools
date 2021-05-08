/* Logic Analyzer Websocket interface

   This takes an 8x1 stream on stdin (e.g. what comes out of the
   Saleae Logic8), pushes it into a circular buffer, keeps track of a
   mipmap representation, and provides access to the data via a web
   interface.

   The JavaScript part is found elsewhere.
*/

//#define MMAP_FILE_LOG LOG
#include "mod_test_minmax.c"
#include "mod_webserver.c"
#include "mod_websocket_leb128s.c"


// ws = new WebSocket("ws://10.1.3.29:3456");
// see test_websocket_timeseries.sh
int main(int argc, char **argv) {

    if (argc != 3) {
        LOG("usage: %s <documentroot> <test.raw>\n", argv[0]);
        return 1;
    }
    ASSERT_ERRNO(chdir(argv[1]));
    minmax_open(&map, argv[2], 8);
    webserver_loop(3456);
}
