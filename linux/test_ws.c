/* TCP server tests.

   This is an example of a modular websocket application.

   - mod_test_minmax bridges the model (minmax wave viewer) to a
     tag_u32 directory handler.

   - mod_webserver implements a tcp web server, parameterized by a
     websocket message handler.

   - mod_websocket_leb128s implements a websocket message handler,
     bridging the leb128 streaming protocol to a tag_u32 directory
     handler.

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
